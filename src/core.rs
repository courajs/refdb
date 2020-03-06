use std::{fmt, fmt::Display};

use nom::{bytes::complete::take, combinator::map, IResult};
use sha3::{Digest, Sha3_256};

pub trait Serializable {
    fn bytes_into(&self, v: &mut Vec<u8>);

    fn bytes(&self) -> Vec<u8> {
        let mut v = Vec::new();
        self.bytes_into(&mut v);
        v
    }
}

pub trait Decodable: Sized {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Self>;
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Hash(pub [u8; 32]);
impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl Hash {
    pub fn sure_from(value: &[u8]) -> Hash {
        let mut val = [0; 32];
        val.copy_from_slice(value);
        Hash(val)
    }

    pub fn of(bytes: &[u8]) -> Hash {
        let mut hasher = Sha3_256::new();
        hasher.input(bytes);

        let mut val = [0; 32];
        val.copy_from_slice(hasher.result().as_ref());
        Hash(val)
    }
}

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sha-256:{}", hex::encode(self.0))
    }
}
impl Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", hex::encode(&self.0[..4]))
    }
}

impl Serializable for Hash {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        v.extend_from_slice(&self.0);
    }
}
impl Decodable for Hash {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Hash> {
        map(take(32u8), Hash::sure_from)(bytes)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Blob {
    pub bytes: Vec<u8>,
}

impl fmt::Debug for Blob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        if self.bytes.len() <= 32 {
            write!(f, "Blob(len={})<0x", self.bytes.len())?;
            for byte in &self.bytes {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, ">")?;
        } else {
            write!(f, "Blob(len={})<0x", self.bytes.len())?;
            for byte in &self.bytes[..16] {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, " ... ")?;
            for byte in &self.bytes[self.bytes.len() - 16..] {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Serializable for Blob {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        v.extend_from_slice(&self.bytes);
    }
}

impl Decodable for Blob {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Blob> {
        Ok((
            &[],
            Blob {
                bytes: bytes.into(),
            },
        ))
    }
}

#[macro_export]
macro_rules! sure {
    ($target:expr, $p:pat => $res:expr; $else:expr) => {
        match $target {
            $p => $res,
            _ => $else
        }
    };
    ($target:expr, $p:pat => $res:expr) => {
        sure!($target, $p => $res; panic!("Expected {} to match pattern: {}", stringify!($target), stringify!($p)))
    };

    ($target:expr, $pat:tt; $else:expr) => {
        sure!($target, $pat => $pat; $else)
    };
    ($target:expr, $pat:tt) => {
        sure!($target, $pat => $pat)
    };

    (let $pat:tt = $target:expr; $else:expr) => {
        let $pat = sure!($target, $pat; $else);
    };
    (let $pat:tt = $target:expr) => {
        let $pat = sure!($target, $pat);
    };
}


