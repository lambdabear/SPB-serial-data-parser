/// Parse received data from power controller board.
/// Data format using NMEA 0183 data specification.
///
/// 数据帧以'$'为起始字符，以<LFOk(*right_val)字符，','为字段分割符，*字符后接校验位。
/// 1.开关量'x':以'1'表示开关闭合，'0'表示开关断开。
///   数字量'x':表示'0'-'9'的数字。
/// 2.控制板循环发送以下状态：
/// (1)输入开关量状态：
///   "IN1:x1;IN2:x2;IN3:x3;ACI:x4;"
/// 说明：分别对应三个输入和市电有无
/// 消息格式："$IN,x1,x2,x3,x4*c\n"，c为校验位，由两个十六进制数字字符构成。(校验位可选?)
/// 例：如发送数据"IN1:1;IN2:0;IN3:1;AC:1;"，则对应数据帧为"$IN,1,0,1,1\n"
/// (2)输出开关量状态：
///   "NO1:x1;NO2:x2;NO3:x3;NO4:x4;NC5:x5;NC6:x6;\n"
/// 说明：前6项分别对应6个输出。
/// 对应消息格式："$NO,x1,x2,x3,x4,x5,x6*c\n"
/// (3)UPS电源输入状态：
///   "UPS V:xxxV;UPS I:xx.xA;UPS P:xxxW;UPS OV:x;\n"
/// 说明：分别对应UPS的电压，电流，功率，超压否。
/// 对应消息格式："$UPS,xxx,xx.x,xxx,x*c\n"
/// (4)电池状态：
///   "BT V:xx.xV;BT I:+xx.xA;BT C:xx%;\n"
/// 说明：分别对应电池电压，电流，容量百分比。
/// 对应消息格式："$BT,xx.x,xx.x,xx*c\n"
/// (5)输出直流状态：
///   "DC V:xx.xV;DC1 I:xx.xA;DC2 I:xx.xA;\n"
/// 说明：分别对应输出电压，输出1电流，输出2电流
/// 对应消息格式："$DC,xx.x,xx.x,xx.x*c\n"
use std::{error::Error, fmt, str::from_utf8};

#[derive(Debug)]
pub struct SwIn {
    in1: bool,
    in2: bool,
    in3: bool,
    ac: bool,
}

impl SwIn {
    pub fn new(in1: bool, in2: bool, in3: bool, ac: bool) -> SwIn {
        SwIn { in1, in2, in3, ac }
    }
}

#[derive(Debug)]
pub struct SwOut {
    out1: bool,
    out2: bool,
    out3: bool,
    out4: bool,
    out5: bool,
    out6: bool,
}

impl SwOut {
    pub fn new(out1: bool, out2: bool, out3: bool, out4: bool, out5: bool, out6: bool) -> SwOut {
        SwOut {
            out1,
            out2,
            out3,
            out4,
            out5,
            out6,
        }
    }
}

#[derive(Debug)]
pub struct Ups {
    voltage: f32,
    current: f32,
    power: f32,
    overvoltage: bool,
}

impl Ups {
    pub fn new(voltage: f32, current: f32, power: f32, overvoltage: bool) -> Ups {
        Ups {
            voltage,
            current,
            power,
            overvoltage,
        }
    }
}

#[derive(Debug)]
pub struct Battery {
    voltage: f32,
    current: f32,
    capacity: u8,
}

impl Battery {
    pub fn new(voltage: f32, current: f32, capacity: u8) -> Battery {
        Battery {
            voltage,
            current,
            capacity,
        }
    }
}

#[derive(Debug)]
pub struct DcOut {
    voltage: f32,
    current1: f32,
    current2: f32,
}

impl DcOut {
    pub fn new(v: f32, c1: f32, c2: f32) -> DcOut {
        DcOut {
            voltage: v,
            current1: c1,
            current2: c2,
        }
    }
}

#[derive(Debug)]
pub enum SpbState {
    SwIn(SwIn),
    SwOut(SwOut),
    Ups(Ups),
    Bt(Battery),
    Dc(DcOut),
}

impl PartialEq for SpbState {
    fn eq(&self, other: &SpbState) -> bool {
        match (self, other) {
            (SpbState::SwIn(s), SpbState::SwIn(o)) => {
                s.in1 == o.in1 && s.in2 == o.in2 && s.in3 == o.in3 && s.ac == o.ac
            }
            (SpbState::SwOut(s), SpbState::SwOut(o)) => {
                s.out1 == o.out1
                    && s.out2 == o.out2
                    && s.out3 == o.out3
                    && s.out4 == o.out4
                    && s.out5 == s.out5
                    && s.out6 == o.out6
            }
            (SpbState::Ups(s), SpbState::Ups(o)) => {
                s.voltage == o.voltage
                    && s.current == o.current
                    && s.power == o.power
                    && s.overvoltage == o.overvoltage
            }
            (SpbState::Bt(s), SpbState::Bt(o)) => {
                s.voltage == o.voltage && s.current == o.current && s.capacity == o.capacity
            }
            (SpbState::Dc(s), SpbState::Dc(o)) => {
                s.voltage == o.voltage && s.current1 == o.current1 && s.current2 == o.current2
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct InvalidDataError;

impl fmt::Display for InvalidDataError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid data")
    }
}

impl Error for InvalidDataError {
    fn description(&self) -> &str {
        "Invalid data"
    }
}

#[derive(Debug)]
pub struct ChecksumError;

impl fmt::Display for ChecksumError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Checksum is incorrect")
    }
}

impl Error for ChecksumError {
    fn description(&self) -> &str {
        "checksum is incorrect"
    }
}

// parse NMEA 0183 data to structured SpbState data
pub fn parse(data: &[u8]) -> Result<SpbState, Box<dyn Error>> {
    let len = data.len();

    if len < 10 || data[0] != b'$' || data[len - 1] != b'\n' {
        return Err(Box::new(InvalidDataError {}));
    }

    // if data have checksum, check that the checksum is correct
    if data[len - 4] == b'*' {
        let checksum = from_utf8(&data[len - 3..len - 1])?;
        let checksum = u8::from_str_radix(checksum, 16)?;

        let xor_sum: u8 = data[1..(len - 4)].iter().fold(0, |acc, x| acc ^ x);

        if checksum != xor_sum {
            return Err(Box::new(ChecksumError {}));
        }
    }

    // get data from data frame, strip header, checksum and tail of the frame
    let mut data_iter = data[1..(len - 1)].split(|x| *x == b'*');

    // parse data to SpbState type
    match data_iter.next() {
        Some(data) => {
            // dividing data segments
            let mut iter = data.split(|x| *x == b',');

            // construct SpbState type data based on data identifiers
            match iter.next() {
                Some(title) => {
                    let data_type = from_utf8(title)?;
                    match data_type {
                        "IN" => {
                            let mut args: Vec<bool> = vec![];
                            for x in iter {
                                let t = from_utf8(x)?.parse::<u8>()?;
                                if t == 0 || t == 1 {
                                    args.push(t != 0)
                                }
                            }
                            match args.len() {
                                4 => Ok(SpbState::SwIn(SwIn::new(
                                    args[0], args[1], args[2], args[3],
                                ))),
                                _ => Err(Box::new(InvalidDataError)),
                            }
                        }
                        "NO" => {
                            let mut args: Vec<bool> = vec![];
                            for x in iter {
                                let t = from_utf8(x)?.parse::<u8>()?;
                                if t == 0 || t == 1 {
                                    args.push(t != 0)
                                }
                            }
                            match args.len() {
                                6 => Ok(SpbState::SwOut(SwOut::new(
                                    args[0], args[1], args[2], args[3], args[4], args[5],
                                ))),
                                _ => Err(Box::new(InvalidDataError)),
                            }
                        }
                        "UPS" => {
                            let mut args1: Vec<f32> = vec![];
                            let mut args2: Vec<bool> = vec![];
                            for (i, x) in iter.enumerate() {
                                match i {
                                    0...2 => {
                                        let t = from_utf8(x)?.parse::<f32>()?;
                                        args1.push(t);
                                    }
                                    3 => {
                                        let t = from_utf8(x)?.parse::<u8>()?;
                                        if t == 0 || t == 1 {
                                            args2.push(t != 0)
                                        }
                                    }
                                    _ => (),
                                }
                            }
                            match (args1.len(), args2.len()) {
                                (3, 1) => Ok(SpbState::Ups(Ups::new(
                                    args1[0], args1[1], args1[2], args2[0],
                                ))),
                                _ => Err(Box::new(InvalidDataError)),
                            }
                        }
                        "BT" => {
                            let mut args1: Vec<f32> = vec![];
                            let mut args2: Vec<u8> = vec![];
                            for (i, x) in iter.enumerate() {
                                match i {
                                    0...1 => {
                                        let t = from_utf8(x)?.parse::<f32>()?;
                                        args1.push(t);
                                    }
                                    2 => {
                                        let t = from_utf8(x)?.parse::<u8>()?;
                                        args2.push(t)
                                    }
                                    _ => (),
                                }
                            }
                            match (args1.len(), args2.len()) {
                                (2, 1) => {
                                    Ok(SpbState::Bt(Battery::new(args1[0], args1[1], args2[0])))
                                }
                                _ => Err(Box::new(InvalidDataError)),
                            }
                        }
                        "DC" => {
                            let mut args: Vec<f32> = vec![];
                            for x in iter {
                                let t = from_utf8(x)?.parse::<f32>()?;
                                args.push(t);
                            }
                            match args.len() {
                                3 => Ok(SpbState::Dc(DcOut::new(args[0], args[1], args[2]))),
                                _ => Err(Box::new(InvalidDataError)),
                            }
                        }
                        _ => Err(Box::new(InvalidDataError)),
                    }
                }
                None => Err(Box::new(InvalidDataError)),
            }
        }
        None => Err(Box::new(InvalidDataError {})),
    }
}

// Assembly valid data to message cache from data buffer.
pub fn extract_msg<'a, 'b>(buffer: &'a [u8], msg_cache: &'b mut Vec<u8>) -> Option<&'b [u8]> {
    // if message cache have a complete message, flush message cache
    if msg_cache.len() > 0 && msg_cache[msg_cache.len() - 1] == b'\n' {
        msg_cache.truncate(0);
    }

    if buffer.len() > 0 {
        for x in buffer.iter() {
            if *x == b'$' {
                msg_cache.truncate(0);
                msg_cache.push(*x);
            } else {
                if msg_cache.len() == 0 {
                    break;
                }
                if *x == b'\n' {
                    msg_cache.push(*x);
                    break;
                }
                msg_cache.push(*x)
            }
        }
    }

    if msg_cache.len() > 2 && msg_cache[0] == b'$' && msg_cache[msg_cache.len() - 1] == b'\n' {
        Some(&msg_cache[..])
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let data_in = format!("$IN,1,0,1,1*{:02X}\n", {
            String::from("IN,1,0,1,1")
                .into_bytes()
                .iter()
                .fold(0, |acc, x| acc ^ x)
        });
        let data_no = format!("$NO,0,1,1,0,0,1*{:02X}\n", {
            String::from("NO,0,1,1,0,0,1")
                .into_bytes()
                .iter()
                .fold(0, |acc, x| acc ^ x)
        });
        let data_ups = format!("$UPS,220,3.2,664,0*{:02X}\n", {
            String::from("UPS,220,3.2,664,0")
                .into_bytes()
                .iter()
                .fold(0, |acc, x| acc ^ x)
        });
        let data_bt = format!("$BT,12.3,5.6,46*{:02X}\n", {
            String::from("BT,12.3,5.6,46")
                .into_bytes()
                .iter()
                .fold(0, |acc, x| acc ^ x)
        });
        let data_dc = format!("$DC,11.9,5.4,1.2*{:02X}\n", {
            String::from("DC,11.9,5.4,1.2")
                .into_bytes()
                .iter()
                .fold(0, |acc, x| acc ^ x)
        });
        print!("IN: {}", data_in);
        assert!(match parse(&String::into_bytes(data_in)) {
            Ok(data_in) => {
                data_in
                    == SpbState::SwIn(SwIn {
                        in1: true,
                        in2: false,
                        in3: true,
                        ac: true
                    })
            }
            Err(_) => false,
        });
        print!("NO: {}", data_no);
        assert!(match parse(&String::into_bytes(data_no)) {
            Ok(data_in) => {
                data_in
                    == SpbState::SwOut(SwOut {
                        out1: false,
                        out2: true,
                        out3: true,
                        out4: false,
                        out5: false,
                        out6: true,
                    })
            }
            Err(_) => false,
        });
        print!("UPS: {}", data_ups);
        assert!(match parse(&String::into_bytes(data_ups)) {
            Ok(data_in) => {
                data_in
                    == SpbState::Ups(Ups {
                        voltage: 220.0,
                        current: 3.2,
                        power: 664.0,
                        overvoltage: false,
                    })
            }
            Err(_) => false,
        });
        print!("BT: {}", data_bt);
        assert!(match parse(&String::into_bytes(data_bt)) {
            Ok(data_in) => {
                data_in
                    == SpbState::Bt(Battery {
                        voltage: 12.3,
                        current: 5.6,
                        capacity: 46,
                    })
            }
            Err(_) => false,
        });
        print!("DC: {}", data_dc);
        assert!(match parse(&String::into_bytes(data_dc)) {
            Ok(data_in) => {
                data_in
                    == SpbState::Dc(DcOut {
                        voltage: 11.9,
                        current1: 5.4,
                        current2: 1.2,
                    })
            }
            Err(_) => false,
        });
    }

    #[test]
    fn test_extract_msg() {
        let mut buffer = String::from("NO,0,1,1,0,0").into_bytes();
        let mut cache: Vec<u8> = vec![];

        assert_eq!(None, extract_msg(&buffer, &mut cache));

        buffer = String::from("$IN,1,0,1,1*06\n").into_bytes();
        assert!(match extract_msg(&buffer, &mut cache) {
            Some(s) => s == &buffer[..],
            None => false,
        });

        buffer = String::from("$IN,1,0,").into_bytes();
        assert_eq!(None, extract_msg(&buffer, &mut cache));
        buffer = String::from("1,1*06").into_bytes();
        assert_eq!(None, extract_msg(&buffer, &mut cache));

        buffer = String::from("\n$UPS").into_bytes();
        assert!(match extract_msg(&buffer, &mut cache) {
            Some(s) => s == &String::from("$IN,1,0,1,1*06\n").into_bytes()[..],
            None => false,
        });

        buffer = String::from(",1,1*06\n").into_bytes();
        assert_eq!(None, extract_msg(&buffer, &mut cache));
    }
}