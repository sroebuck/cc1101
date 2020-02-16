use crate::lowlevel::FXOSC;

pub const fn from_frequency(hz: u64) -> (u8, u8, u8) {
    let freq = hz * 1u64.rotate_left(16) / FXOSC;
    let freq0 = (freq & 0xff) as u8;
    let freq1 = ((freq >> 8) & 0xff) as u8;
    let freq2 = ((freq >> 16) & 0xff) as u8;
    (freq0, freq1, freq2)
}

pub const fn to_frequency(f0: u8, f1: u8, f2: u8) -> u64 {
    (f0 as u64 + ((f1 as u64) << 8) + ((f2 as u64) << 16)) * FXOSC / 1u64.rotate_left(16)
}

pub const fn from_deviation(v: u64) -> (u8, u8) {
    let exponent = 64 - (v.rotate_left(14) / FXOSC).leading_zeros() - 1;
    let mantissa = (v.rotate_left(17) / (FXOSC.rotate_left(exponent))) - 7;
    ((mantissa & 0b111) as u8, (exponent & 0b111) as u8)
}

pub const fn to_deviation(mantissa: u8, exponent: u8) -> u64 {
    ((mantissa as u64 + 8) << exponent) * FXOSC / 1u64.rotate_left(17)
}

// TODO: Not defined for all values, need to figure out.
pub const fn from_drate(v: u64) -> (u8, u8) {
    let exponent = 64 - (v.rotate_left(19) / FXOSC).leading_zeros();
    let mantissa = ((v.rotate_left(27)) / (FXOSC.rotate_left(exponent - 1))) - 255;
    // When mantissa is 256, wrap to zero and increase exponent by one
    [(mantissa as u8, exponent as u8), (0u8, (exponent + 1) as u8)][(mantissa == 256) as usize]
}

pub const fn to_drate(mantissa: u8, exponent: u8) -> u64 {
    ((mantissa as u64 + 256) << exponent) * FXOSC / 1u64.rotate_left(28)
}

pub fn from_chanbw(v: u64) -> (u8, u8) {
    let exponent = 64 - (FXOSC / (8 * 4 * v)).leading_zeros() - 1;
    let mantissa = FXOSC / (v * 8 * 2u64.pow(exponent)) - 4;
    (mantissa as u8 & 0x3, exponent as u8 & 0x3)
}

pub const fn to_chanbw(chanbw_m: u8, chanbw_e: u8) -> u64 {
    FXOSC / ((chanbw_m as u64 + 4) << (chanbw_e + 3))
}

pub fn from_chanspc(v: u64) -> (u8, u8) {
    let exponent = 64 - (v.rotate_left(18) / FXOSC).leading_zeros();
    let mantissa = ((v.rotate_left(18)) / (FXOSC.rotate_left(exponent - 1))) - 256;
    // When mantissa is 256, wrap to zero and increase exponent by one
    [(mantissa as u8, exponent as u8), (0u8, (exponent + 1) as u8)][(mantissa == 256) as usize];
    todo!(); // Because the maths is wrong right now!
}

pub const fn to_chanspc(chanspc_m: u8, chanspc_e: u8) -> u64 {
    FXOSC / 1u64.rotate_left(18) * ((256u64 + chanspc_m as u64) << chanspc_e)
}

#[cfg(test)]
mod tests {
    use crate::lowlevel::convert::*;
    use crate::lowlevel::FXOSC;

    #[test]
    fn test_frequency() {
        assert_eq!(from_frequency(433_000_000), (0x62, 0xA7, 0x10));
        assert_eq!(from_frequency(868_000_000), (0x76, 0x62, 0x21));
        assert_eq!(from_frequency(902_000_000), (0x3B, 0xB1, 0x22));
        assert_eq!(from_frequency(918_000_000), (0xC4, 0x4E, 0x23));
    }

    #[test]
    fn test_deviation() {
        // f_dev = f_osc / 2^17 * (8 + DEVIATION_M) * 2^DEVIATION_E
        fn calc_rev_dev(dev_m: u8, dev_e: u8) -> u64 {
            (((FXOSC as f32 / (2u64.pow(17) as f32)) as f32)
                * (8f32 + dev_m as f32)
                * (2u64.pow(dev_e as u32) as f32)) as u64
        }

        for e in 0..7 {
            for m in 1..7 {
                assert_eq!(from_deviation(calc_rev_dev(m, e)), (m, e));
            }
        }
    }

    #[test]
    fn test_drate() {
        // Some sample settings from SmartRF Studio
        assert_eq!((117, 5), from_drate(1_156));
        assert_eq!((117, 7), from_drate(4_624));
        assert_eq!((117, 10), from_drate(36_994));
        assert_eq!((34, 12), from_drate(115_051));
        assert_eq!((59, 14), from_drate(499_877));
        assert_eq!((59, 13), from_drate(249_938));
        assert_eq!((248, 11), from_drate(99_975));
        assert_eq!((131, 11), from_drate(76_766));
        assert_eq!((131, 10), from_drate(38_383));
        assert_eq!((147, 8), from_drate(9_992));
        assert_eq!((131, 7), from_drate(4_797));
        assert_eq!((131, 6), from_drate(2_398));
        assert_eq!((131, 5), from_drate(1_199));

        // fn calc_drate_rev(mantissa: u8, exponent: u8) -> u64 {
        //     let q = (256.0 + mantissa as f64) * 2f64.powf(exponent as f64);
        //     let p = 2f64.powf(28.0);
        //     ((q / p) * FXOSC as f64).floor() as u64
        // }
        for e in 0..255 {
            for m in 0..255 {
                let baud = to_drate(m, e);
                let (mp, ep) = from_drate(baud);
                assert_eq!((mp, ep), (m, e));
            }
        }
    }

    #[test]
    fn test_chanbw() {
        assert_eq!(from_chanbw(812_500), (0b00, 0b00));
        assert_eq!(from_chanbw(650_000), (0b01, 0b00));
        assert_eq!(from_chanbw(541_666), (0b10, 0b00));
        assert_eq!(from_chanbw(464_285), (0b11, 0b00));
        assert_eq!(from_chanbw(406_250), (0b00, 0b01));
        assert_eq!(from_chanbw(325_000), (0b01, 0b01));
        assert_eq!(from_chanbw(270_833), (0b10, 0b01));
        assert_eq!(from_chanbw(232_142), (0b11, 0b01));
        assert_eq!(from_chanbw(203_125), (0b00, 0b10));
        assert_eq!(from_chanbw(162_000), (0b01, 0b10));
        assert_eq!(from_chanbw(135_416), (0b10, 0b10));
        assert_eq!(from_chanbw(116_071), (0b11, 0b10));
        assert_eq!(from_chanbw(101_562), (0b00, 0b11));
        assert_eq!(from_chanbw(81_250), (0b01, 0b11));
        assert_eq!(from_chanbw(67_708), (0b10, 0b11));
        assert_eq!(from_chanbw(58_035), (0b11, 0b11));
        assert_eq!(812_500, to_chanbw(from_chanbw(812_500).0, from_chanbw(812_500).1));
        assert_eq!(270_833, to_chanbw(from_chanbw(270_833).0, from_chanbw(270_833).1));
        assert_eq!(58_035, to_chanbw(from_chanbw(58_035).0, from_chanbw(58_035).1));
    }

    #[test]
    fn test_channel_spacing() {
        assert_eq!(100_000, to_chanspc(from_chanspc(100_000).0, from_chanbw(100_000).1))
    }
}
