// #![no_std]

extern crate embedded_hal as hal;

use hal::blocking::spi::{Transfer, Write};
use hal::digital::v2::OutputPin;

#[macro_use]
pub mod lowlevel;
mod rssi;

use lowlevel::convert::*;
use lowlevel::registers::*;
use lowlevel::types::*;
use rssi::rssi_to_dbm;

/// CC1101 errors.
#[derive(Debug)]
pub enum Error<SpiE, GpioE> {
    /// The RX FIFO buffer overflowed, too small buffer for configured packet length.
    RxOverflow,
    /// Corrupt packet received with invalid CRC.
    CrcMismatch,
    /// Platform-dependent SPI-errors, such as IO errors.
    Spi(SpiE),
    /// Platform-dependent GPIO-errors, such as IO errors.
    Gpio(GpioE),
}

impl<SpiE, GpioE> From<lowlevel::Error<SpiE, GpioE>> for Error<SpiE, GpioE> {
    fn from(e: lowlevel::Error<SpiE, GpioE>) -> Self {
        match e {
            lowlevel::Error::Spi(inner) => Error::Spi(inner),
            lowlevel::Error::Gpio(inner) => Error::Gpio(inner),
        }
    }
}

/// High level API for interacting with the CC1101 radio chip.
pub struct Cc1101<SPI, CS>(lowlevel::Cc1101<SPI, CS>);

impl<SPI, CS, SpiE, GpioE> Cc1101<SPI, CS>
where
    SPI: Transfer<u8, Error = SpiE> + Write<u8, Error = SpiE>,
    CS: OutputPin<Error = GpioE>,
{
    pub fn new(spi: SPI, cs: CS) -> Result<Self, Error<SpiE, GpioE>> {
        Ok(Cc1101(lowlevel::Cc1101::new(spi, cs)?))
    }

    /// Get the frequency setting in Hz.
    pub fn get_frequency(&mut self) -> Result<u64, Error<SpiE, GpioE>> {
        let freq0 = self.0.read_register(Config::FREQ0)?;
        let freq1 = self.0.read_register(Config::FREQ1)?;
        let freq2 = self.0.read_register(Config::FREQ2)?;
        Ok(to_frequency(freq0, freq1, freq2))
    }

    /// Set the frequency in Hz.
    pub fn set_frequency(&mut self, hz: u64) -> Result<(), Error<SpiE, GpioE>> {
        let (freq0, freq1, freq2) = from_frequency(hz);
        self.0.write_register(Config::FREQ0, freq0)?;
        self.0.write_register(Config::FREQ1, freq1)?;
        self.0.write_register(Config::FREQ2, freq2)?;
        Ok(())
    }

    /// Get deviation in Hz.
    pub fn get_deviation(&mut self) -> Result<u64, Error<SpiE, GpioE>> {
        let d = self.0.read_register(Config::DEVIATN)?;
        let deviation_m = d & 0b111;
        let deviation_e = (d >> 4) & 0b111;
        Ok(to_deviation(deviation_m, deviation_e))
    }

    /// Set deviation in Hz.
    pub fn set_deviation(&mut self, deviation: u64) -> Result<(), Error<SpiE, GpioE>> {
        let (mantissa, exponent) = from_deviation(deviation);
        self.0.write_register(
            Config::DEVIATN,
            DEVIATN::default().deviation_m(mantissa).deviation_e(exponent).bits(),
        )?;
        Ok(())
    }

    /// Get data rate in bits per second (baud).
    pub fn get_data_rate(&mut self) -> Result<u64, Error<SpiE, GpioE>> {
        let drate_e = self.0.read_register(Config::MDMCFG4)? & 0b1111;
        let drate_m = self.0.read_register(Config::MDMCFG3)?;
        Ok(to_drate(drate_m, drate_e))
    }

    /// Set data rate in bits per second (baud).
    pub fn set_data_rate(&mut self, baud: u64) -> Result<(), Error<SpiE, GpioE>> {
        let (mantissa, exponent) = from_drate(baud);
        self.0
            .modify_register(Config::MDMCFG4, |r| MDMCFG4(r).modify().drate_e(exponent).bits())?;
        self.0.write_register(Config::MDMCFG3, MDMCFG3::default().drate_m(mantissa).bits())?;
        Ok(())
    }

    /// Get channel bandwidth in Hz.
    pub fn get_chanbw(&mut self) -> Result<u64, Error<SpiE, GpioE>> {
        let mdmcfg4 = self.0.read_register(Config::MDMCFG4)?;
        let chanbw_e = (mdmcfg4 >> 6) & 0b11;
        let chanbw_m = (mdmcfg4 >> 4) & 0b11;
        Ok(to_chanbw(chanbw_m, chanbw_e))
    }

    /// Set channel bandwidth in Hz.
    pub fn set_chanbw(&mut self, bandwidth: u64) -> Result<(), Error<SpiE, GpioE>> {
        let (mantissa, exponent) = from_chanbw(bandwidth);
        self.0.modify_register(Config::MDMCFG4, |r| {
            MDMCFG4(r).modify().chanbw_m(mantissa).chanbw_e(exponent).bits()
        })?;
        Ok(())
    }

    pub fn get_channel_spacing(&mut self) -> Result<u64, Error<SpiE, GpioE>> {
        let chanspc_e = self.0.read_register(Config::MDMCFG1)? & 0b11;
        let chanspc_m = self.0.read_register(Config::MDMCFG0)?;
        Ok(to_chanspc(chanspc_m, chanspc_e))
    }

    pub fn set_channel_spacing(&mut self, channel_spacing: u64) -> Result<(), Error<SpiE, GpioE>> {
        let (mantissa, exponent) = from_chanspc(channel_spacing);
        self.0
            .modify_register(Config::MDMCFG1, |r| MDMCFG1(r).modify().chanspc_e(exponent).bits())?;
        self.0.write_register(Config::MDMCFG0, MDMCFG0::default().chanspc_m(mantissa).bits())?;
        Ok(())
    }

    pub fn get_hw_info(&mut self) -> Result<(u8, u8), Error<SpiE, GpioE>> {
        let partnum = self.0.read_register(Status::PARTNUM)?;
        let version = self.0.read_register(Status::VERSION)?;
        Ok((partnum, version))
    }

    /// Received Signal Strength Indicator is an estimate of the signal power level in the chosen channel.
    pub fn get_rssi_dbm(&mut self) -> Result<i16, Error<SpiE, GpioE>> {
        Ok(rssi_to_dbm(self.0.read_register(Status::RSSI)?))
    }

    /// The Link Quality Indicator metric of the current quality of the received signal.
    pub fn get_lqi(&mut self) -> Result<u8, Error<SpiE, GpioE>> {
        let lqi = self.0.read_register(Status::LQI)?;
        Ok(lqi & !(1u8 << 7))
    }

    pub fn get_max_dvga_gain(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let agcctrl2 = self.0.read_register(Config::AGCCTRL2)?;
        let max_dvga_gain = match agcctrl2 >> 6 & 0b11 {
            0b00 => "All gain settings can be used",
            0b01 => "The highest gain setting can not be used",
            0b10 => "The 2 highest gain settings can not be used",
            0b11 => "The 3 highest gain settings can not be used",
            _ => unreachable!(),
        };
        Ok(max_dvga_gain.to_string())
    }

    pub fn get_max_lna_gain(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let agcctrl2 = self.0.read_register(Config::AGCCTRL2)?;
        let get_max_lna_gain = match agcctrl2 >> 3 & 0b111 {
            0b000 => "Maximum possible LNA + LNA 2 gain",
            0b001 => "Approx. 2.6 dB below maximum possible gain",
            0b010 => "Approx. 6.1 dB below maximum possible gain",
            0b011 => "Approx. 7.4 dB below maximum possible gain",
            0b100 => "Approx. 9.2 dB below maximum possible gain",
            0b101 => "Approx. 11.5 dB below maximum possible gain",
            0b110 => "Approx. 14.6 dB below maximum possible gain",
            0b111 => "Approx. 17.1 dB below maximum possible gain",
            _ => unreachable!(),
        };
        Ok(get_max_lna_gain.to_string())
    }

    pub fn get_magn_target(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let agcctrl2 = self.0.read_register(Config::AGCCTRL2)?;
        let get_magn_target = match agcctrl2 & 0b111 {
            0b000 => "24 dB",
            0b001 => "27 dB",
            0b010 => "30 dB",
            0b011 => "33 dB",
            0b100 => "36 dB",
            0b101 => "38 dB",
            0b110 => "40 dB",
            0b111 => "42 dB",
            _ => unreachable!(),
        };
        Ok(get_magn_target.to_string())
    }

    pub fn get_sync_mode(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let mdmcfg2 = self.0.read_register(Config::MDMCFG2)?;
        let sync0 = self.0.read_register(Config::SYNC0)?;
        let sync1 = self.0.read_register(Config::SYNC1)?;
        let sync_mode = match mdmcfg2 & 0b111 {
            0b000 => "No preamble / sync".to_string(),
            0b001 => format!("15 sync word bits detected: {:08b}{:08b}", sync1, sync0),
            0b010 => format!("16 sync word bits detected: {:08b}{:08b}", sync1, sync0),
            0b011 => format!("30 sync word bits detected: {:08b}{:08b}", sync1, sync0),
            0b100 => "No preamble/sync, carrier-sense above threshold".to_string(),
            0b101 => format!("15 + carrier-sense above threshold: {:08b}{:08b}", sync1, sync0),
            0b110 => format!("16 + carrier-sense above threshold: {:08b}{:08b}", sync1, sync0),
            0b111 => format!("30 + carrier-sense above threshold: {:08b}{:08b}", sync1, sync0),
            _ => unreachable!(),
        };

        Ok(sync_mode)
    }

    /// Configure the sync word to use, and at what level it should be verified.
    pub fn set_sync_mode(&mut self, sync_mode: SyncMode) -> Result<(), Error<SpiE, GpioE>> {
        let reset: u16 = (SYNC1::default().bits() as u16) << 8 | (SYNC0::default().bits() as u16);

        let (mode, word) = match sync_mode {
            // TODO: The CHECK_0_0_CS mode is required for this to work at all.
            // This has been temporarily changed to see if CHECK_0_0_CS is the setting needed to
            // detect signals that don't have any sync header.
            // SyncMode::Disabled => (SyncCheck::DISABLED, reset),
            SyncMode::Disabled => (SyncCheck::CHECK_0_0_CS, reset),
            SyncMode::MatchPartial(word) => (SyncCheck::CHECK_15_16, word),
            SyncMode::MatchPartialRepeated(word) => (SyncCheck::CHECK_30_32, word),
            SyncMode::MatchFull(word) => (SyncCheck::CHECK_16_16, word),
        };
        self.0.modify_register(Config::MDMCFG2, |r| {
            MDMCFG2(r).modify().sync_mode(mode.value()).bits()
        })?;
        self.0.write_register(Config::SYNC1, ((word >> 8) & 0xff) as u8)?;
        self.0.write_register(Config::SYNC0, (word & 0xff) as u8)?;
        Ok(())
    }

    pub fn get_modulation(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let mdmcfg2 = self.0.read_register(Config::MDMCFG2)?;
        let modulation_format = match mdmcfg2 >> 4 & 0b111 {
            0b000 => "2-FSK",
            0b001 => "GFSK",
            0b011 => "ASK/OOK",
            0b100 => "4-FSK",
            0b111 => "MSK",
            _ => "-",
        };
        Ok(modulation_format.to_string())
    }

    /// Configure signal modulation.
    pub fn set_modulation(&mut self, format: Modulation) -> Result<(), Error<SpiE, GpioE>> {
        use lowlevel::types::ModFormat as MF;

        let value = match format {
            Modulation::BinaryFrequencyShiftKeying => MF::MOD_2FSK,
            Modulation::GaussianFrequencyShiftKeying => MF::MOD_GFSK,
            Modulation::OnOffKeying => MF::MOD_ASK_OOK,
            Modulation::FourFrequencyShiftKeying => MF::MOD_4FSK,
            Modulation::MinimumShiftKeying => MF::MOD_MSK,
        };
        self.0.modify_register(Config::MDMCFG2, |r| {
            MDMCFG2(r).modify().mod_format(value.value()).bits()
        })?;
        Ok(())
    }

    /// Configure device address, and address filtering.
    pub fn set_address_filter(&mut self, filter: AddressFilter) -> Result<(), Error<SpiE, GpioE>> {
        use lowlevel::types::AddressCheck as AC;

        let (mode, addr) = match filter {
            AddressFilter::Disabled => (AC::DISABLED, ADDR::default().bits()),
            AddressFilter::Device(addr) => (AC::SELF, addr),
            AddressFilter::DeviceLowBroadcast(addr) => (AC::SELF_LOW_BROADCAST, addr),
            AddressFilter::DeviceHighLowBroadcast(addr) => (AC::SELF_HIGH_LOW_BROADCAST, addr),
        };
        self.0.modify_register(Config::PKTCTRL1, |r| {
            PKTCTRL1(r).modify().adr_chk(mode.value()).bits()
        })?;
        self.0.write_register(Config::ADDR, addr)?;
        Ok(())
    }

    pub fn get_packet_length(&mut self) -> Result<String, Error<SpiE, GpioE>> {
        let pktctrl0 = self.0.read_register(Config::PKTCTRL0)?;
        let pktlen = self.0.read_register(Config::PKTLEN)?;
        let packet_length = match pktctrl0 & 0b11 {
            0b00 => format!("Fixed packet length of {} bytes", pktlen),
            0b01 => "Variable packet length configured by first byte after sync word".to_string(),
            0b10 => "Infinite packet length mode".to_string(),
            _ => "-".to_string(),
        };
        Ok(packet_length)
    }

    /// Configure packet mode, and length.
    pub fn set_packet_length(&mut self, length: PacketLength) -> Result<(), Error<SpiE, GpioE>> {
        use lowlevel::types::LengthConfig as LC;

        let (format, pktlen) = match length {
            PacketLength::Fixed(limit) => (LC::FIXED, limit),
            PacketLength::Variable(max_limit) => (LC::VARIABLE, max_limit),
            PacketLength::Infinite => (LC::INFINITE, PKTLEN::default().bits()),
        };
        self.0.modify_register(Config::PKTCTRL0, |r| {
            PKTCTRL0(r).modify().length_config(format.value()).bits()
        })?;
        self.0.write_register(Config::PKTLEN, pktlen)?;
        Ok(())
    }

    /// Set radio in Receive/Transmit/Idle mode.
    pub fn set_radio_mode(&mut self, radio_mode: RadioMode) -> Result<(), Error<SpiE, GpioE>> {
        let target = match radio_mode {
            RadioMode::Receive => {
                self.set_radio_mode(RadioMode::Idle)?;
                self.0.write_strobe(Command::SRX)?;
                MachineState::RX
            }
            RadioMode::Transmit => {
                self.set_radio_mode(RadioMode::Idle)?;
                self.0.write_strobe(Command::STX)?;
                MachineState::TX
            }
            RadioMode::Idle => {
                self.0.write_strobe(Command::SIDLE)?;
                MachineState::IDLE
            }
        };
        self.await_machine_state(target)
    }

    /// Configure some default settings, to be removed in the future.
    #[rustfmt::skip]
    pub fn set_defaults(&mut self) -> Result<(), Error<SpiE, GpioE>> {
        self.0.write_strobe(Command::SRES)?;

        let _ = self.write_registers(&mut std::io::stdout());
        let _ = self.write_settings(&mut std::io::stdout());

        self.0.write_register(Config::PKTCTRL0, PKTCTRL0::default()
            .white_data(0).bits()
        )?;

        self.0.write_register(Config::FSCTRL1, FSCTRL1::default()
            .freq_if(0x08).bits() // f_if = (f_osc / 2^10) * FREQ_IF
        )?;

        self.0.write_register(Config::MDMCFG2, MDMCFG2::default()
            .dem_dcfilt_off(1).bits()
        )?;

        self.0.write_register(Config::MCSM0, MCSM0::default()
            .fs_autocal(AutoCalibration::FROM_IDLE.value()).bits()
        )?;

        self.0.write_register(Config::AGCCTRL2, AGCCTRL2::default()
            .max_lna_gain(0x04).bits()
        )?;

        Ok(())
    }

    fn await_machine_state(&mut self, target: MachineState) -> Result<(), Error<SpiE, GpioE>> {
        loop {
            let marcstate = MARCSTATE(self.0.read_register(Status::MARCSTATE)?);
            if target.value() == marcstate.marc_state() {
                break;
            }
        }
        Ok(())
    }

    fn rx_bytes_available(&mut self) -> Result<u8, Error<SpiE, GpioE>> {
        let mut last = 0;

        loop {
            let rxbytes = RXBYTES(self.0.read_register(Status::RXBYTES)?);
            if rxbytes.rxfifo_overflow() == 1 {
                return Err(Error::RxOverflow);
            }

            let nbytes = rxbytes.num_rxbytes();
            if nbytes > 0 && nbytes == last {
                break;
            }

            last = nbytes;
        }
        Ok(last)
    }

    // Should also be able to configure MCSM1.RXOFF_MODE to declare what state
    // to enter after fully receiving a packet.
    // Possible targets: IDLE, FSTON, TX, RX
    pub fn receive(&mut self, addr: &mut u8, buf: &mut [u8]) -> Result<u8, Error<SpiE, GpioE>> {
        match self.rx_bytes_available() {
            Ok(_nbytes) => {
                let mut length = 0u8;
                self.0.read_fifo(addr, &mut length, buf)?;
                let lqi = self.0.read_register(Status::LQI)?;
                self.await_machine_state(MachineState::IDLE)?;
                self.0.write_strobe(Command::SFRX)?;
                if (lqi >> 7) != 1 {
                    Err(Error::CrcMismatch)
                } else {
                    Ok(length)
                }
            }
            Err(err) => {
                self.0.write_strobe(Command::SFRX)?;
                Err(err)
            }
        }
    }

    pub fn receive_without_crc(
        &mut self,
        addr: &mut u8,
        buf: &mut [u8],
    ) -> Result<(u8, u8), Error<SpiE, GpioE>> {
        match self.rx_bytes_available() {
            Ok(_nbytes) => {
                let mut length = 0u8;
                self.0.read_fifo(addr, &mut length, buf)?;
                let lqi = self.0.read_register(Status::LQI)?;
                self.await_machine_state(MachineState::IDLE)?;
                self.0.write_strobe(Command::SFRX)?;
                Ok((length, lqi))
            }
            Err(err) => {
                self.0.write_strobe(Command::SFRX)?;
                Err(err)
            }
        }
    }

    pub fn write_registers(
        &mut self,
        w: &mut impl std::io::Write,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let registers = [
            Config::IOCFG0,
            Config::FIFOTHR,
            Config::PKTCTRL1,
            Config::PKTCTRL0,
            Config::ADDR,
            Config::CHANNR,
            Config::FSCTRL1,
            Config::FSCTRL0,
            Config::FREQ2,
            Config::FREQ1,
            Config::FREQ0,
            Config::MDMCFG4,
            Config::MDMCFG3,
            Config::MDMCFG2,
            Config::MDMCFG1,
            Config::MDMCFG0,
            Config::DEVIATN,
            Config::MCSM2,
            Config::MCSM1,
            Config::MCSM0,
            Config::FOCCFG,
            Config::BSCFG,
            Config::AGCCTRL2,
            Config::AGCCTRL1,
            Config::AGCCTRL0,
            Config::WOREVT1,
            Config::WOREVT0,
            Config::WORCTRL,
            Config::FREND1,
            Config::FREND0,
            Config::FSCAL3,
            Config::FSCAL2,
            Config::FSCAL1,
            Config::FSCAL0,
            Config::RCCTRL1,
            Config::RCCTRL0,
            Config::FSTEST,
            Config::PTEST,
            Config::AGCTEST,
            Config::TEST2,
            Config::TEST1,
            Config::TEST0,
        ];
        let register_names = [
            "IOCFG0", "FIFOTHR", "PKTCTRL1", "PKTCTRL0", "ADDR", "CHANNR", "FSCTRL1", "FSCTRL0",
            "FREQ2", "FREQ1", "FREQ0", "MDMCFG4", "MDMCFG3", "MDMCFG2", "MDMCFG1", "MDMCFG0",
            "DEVIATN", "MCSM2", "MCSM1", "MCSM0", "FOCCFG", "BSCFG", "AGCCTRL2", "AGCCTRL1",
            "AGCCTRL0", "WOREVT1", "WOREVT0", "WORCTRL", "FREND1", "FREND0", "FSCAL3", "FSCAL2",
            "FSCAL1", "FSCAL0", "RCCTRL1", "RCCTRL0", "FSTEST", "PTEST", "AGCTEST", "TEST2",
            "TEST1", "TEST0",
        ];
        assert_eq!(registers.len(), register_names.len());
        let results: Vec<u8> =
            registers.iter().map(|&r| self.0.read_register(r).unwrap_or(0u8)).collect();
        let mapping: Vec<(&&str, u8)> = register_names.iter().zip(results).collect();
        mapping
            .iter()
            .for_each(|(k, v)| writeln!(w, "  {:<8} : {:>08b} (0x{:>02X})", k, v, v).unwrap());

        Ok(())
    }

    pub fn write_settings(
        &mut self,
        w: &mut impl std::io::Write,
    ) -> Result<(), Error<SpiE, GpioE>> {
        let (partnum, version) = self.get_hw_info()?;
        let _ = writeln!(w, "Part number = {}", partnum);
        let _ = writeln!(w, "Version = {}", version);
        let _ = writeln!(w, "Frequency = {:0.3} MHz", (self.get_frequency()? as f64) / 1_000_000.0);
        let _ = writeln!(w, "Deviation = {:0.3} kHz", (self.get_deviation()? as f64) / 1_000.0);
        let _ = writeln!(w, "Data rate = {} Baud", self.get_data_rate()?);
        let _ = writeln!(w, "Channel bandwidth = {} kHz", (self.get_chanbw()? as f64) / 1_000.0);
        let _ = writeln!(w, "Modulation format = {}", self.get_modulation()?);
        let _ = writeln!(w, "Sync mode = {}", self.get_sync_mode()?);
        let _ = writeln!(w, "Packet length = {}", self.get_packet_length()?);
        let _ =
            writeln!(w, "Channel spacing = {} kHz", (self.get_channel_spacing()? as f64) / 1_000.0);
        let _ = writeln!(w, "Max allowable DVGA gain = {}", self.get_max_dvga_gain()?);
        let _ = writeln!(w, "Max allowable LNA + LNA2 gain = {}", self.get_max_lna_gain()?);
        let _ = writeln!(w, "Average amplitude target = {}", self.get_magn_target()?);
        Ok(())
    }

    /// Additional settings set up to try and identify what missing settings are required to make
    /// RX work for the radiator signal.
    pub fn additional_settings(&mut self) -> Result<(), Error<SpiE, GpioE>> {
        // UNLIKELY TO NEED CHANGING...

        // Output pin configuration
        self.0.write_register(Config::IOCFG0, 0b0_0_000000)?;
        // No preamble quality check, no address check
        // The preamble quality check cannot be used because, by using a frequency of 5000baud to
        // handle the way that messages are encoded, we will expect double 1 double 0
        // sequences which will constantly reset the preamble quality check rather than raising it.
        self.0.write_register(Config::PKTCTRL1, 0b000_0_0_0_00)?;
        // No whitening, no CRC check, infinite packet length
        self.0.write_register(Config::PKTCTRL0, 0b0_0_00_0_0_10)?;
        // Frequency synthesizer controls. This hasn't made any difference and it shouldn't because
        // it should be the distance between channels and we aren't using channels.
        self.0.write_register(Config::FSCTRL1, 0x06)?;
        // Frequency synthesizer controls2 - leave be right now!
        self.0.write_register(Config::FSCTRL0, 0x00)?;
        // Channel bandwidth - from SMARTRF recommendations...
        self.0.write_register(Config::MDMCFG4, 0xF7)?;
        self.0.write_register(Config::MDMCFG3, 0x93)?;
        // Default settings for 2 preamble bits and channel spacing
        self.0.write_register(Config::MDMCFG1, 0x22)?;
        // Channel spacing mantissa - default values
        self.0.write_register(Config::MDMCFG0, 0xF8)?;
        // SMARTRF Recommendation but shouldn't be used for RX
        self.0.write_register(Config::DEVIATN, 0x15)?;
        // This seems to matter and this setting works well.  In theory there is no other settings
        // for ASK/OOP.
        self.0.write_register(Config::FOCCFG, 0b00_1_00_0_00)?;

        // POTENTIALLY WORTH

        // Close in RX 0dB, RX FIFO=36 RX bytes, 29 TX bytes
        // This also sets the RX attenuation.  It has been suggested in DN010 that the CC1101 can
        // be too sensitive at short distance and that setting this can help to compensate.  I
        // tried all the settings and it's not clear that any one was better than another.
        self.0.write_register(Config::FIFOTHR, 0b0_0_10_1000)?;

        // Better sensitivity, ASK/OOK, No manchester, No preample/sync, carrier sense above threshold
        // This is overwritten in part by set sync mode function
        self.0.write_register(Config::MDMCFG2, 0b0_011_0_100)?;

        // RX terminal timeouts
        // bit 4 = 0 seems to be critical to making this work.
        // May be worth playing with the bottom 3 bits but my tests didn't show any difference.
        self.0.write_register(Config::MCSM2, 0b000_0_0_111)?;

        // Clear channel assessment (for TX), IDLE after RX, IDLE after TX
        // I don't think this should have any impact because we aren't transmitting and we aren't
        // using packets.
        self.0.write_register(Config::MCSM1, 0b00_00_00_00)?;

        // Calibrate going from IDLE to RX, stablize time high
        // IDLE to RX calibration seems to beet RX to IDLE.
        self.0.write_register(Config::MCSM0, 0b00_01_11_0_0)?;

        // Bit synchronization configuration
        self.0.write_register(Config::BSCFG, 0b01_10_1_1_01)?;

        //  Target value = 33dB works well at the moment.
        self.0.write_register(Config::AGCCTRL2, 0b00_000_011)?;
        // Automatic gain control
        self.0.write_register(Config::AGCCTRL1, 0b0_0_01_0001)?;
        // Gain control...
        self.0.write_register(Config::AGCCTRL0, 0b01_11_00_01)?;
        // Wake on Radio Control settings - set to stay on at the moment.
        self.0.write_register(Config::WORCTRL, 0b1_111_1_0_11)?;
        // ?
        self.0.write_register(Config::FREND1, 0x56)?;
        // PATABLE index of 1 when transmitting 1 in OOK
        self.0.write_register(Config::FREND0, 0b00_01_0_001)?;
        // Settings suggested by SMARTRF - currently they break reception!
        // self.0.write_register(Config::FSCAL3, 0xE9)?;
        // self.0.write_register(Config::FSCAL2, 0x2A)?;
        // self.0.write_register(Config::FSCAL1, 0x00)?;
        // self.0.write_register(Config::FSCAL0, 0x1F)?;
        // Settings suggested by SMARTRF - use unknown
        self.0.write_register(Config::TEST2, 0x81)?;
        self.0.write_register(Config::TEST1, 0x35)?;
        self.0.write_register(Config::TEST0, 0x09)?;
        Ok(())
    }
}

/// Modulation format configuration.
pub enum Modulation {
    /// 2-FSK.
    BinaryFrequencyShiftKeying,
    /// GFSK.
    GaussianFrequencyShiftKeying,
    /// ASK / OOK.
    OnOffKeying,
    /// 4-FSK.
    FourFrequencyShiftKeying,
    /// MSK.
    MinimumShiftKeying,
}

/// Packet length configuration.
pub enum PacketLength {
    /// Set packet length to a fixed value.
    Fixed(u8),
    /// Set upper bound of variable packet length.
    Variable(u8),
    /// Infinite packet length, streaming mode.
    Infinite,
}

/// Address check configuration.
pub enum AddressFilter {
    /// No address check.
    Disabled,
    /// Address check, no broadcast.
    Device(u8),
    /// Address check and 0 (0x00) broadcast.
    DeviceLowBroadcast(u8),
    /// Address check and 0 (0x00) and 255 (0xFF) broadcast.
    DeviceHighLowBroadcast(u8),
}

/// Radio operational mode.
pub enum RadioMode {
    Receive,
    Transmit,
    Idle,
}

/// Sync word configuration.
pub enum SyncMode {
    /// No sync word.
    Disabled,
    /// Match 15 of 16 bits of given sync word.
    MatchPartial(u16),
    /// Match 30 of 32 bits of a repetition of given sync word.
    MatchPartialRepeated(u16),
    /// Match 16 of 16 bits of given sync word.
    MatchFull(u16),
}
