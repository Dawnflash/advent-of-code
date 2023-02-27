use nom::{
    character::complete::{char, digit1},
    combinator::{map_res, opt, recognize},
    sequence::preceded,
};
use std::{
    ops::{Add, Sub},
    str::FromStr,
};

pub fn parse_int<I>(i: &str) -> nom::IResult<&str, I>
where
    I: FromStr,
{
    map_res(recognize(preceded(opt(char('-')), digit1)), I::from_str)(i)
}

// 2D shenanigans
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Point2D {
    pub x: i32,
    pub y: i32,
}

#[macro_export]
macro_rules! index_2d {
    ($v: expr, $p: expr) => {
        $v[$p.y as usize][$p.x as usize]
    };
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Direction2D {
    U,
    UR,
    R,
    DR,
    D,
    DL,
    L,
    UL,
}

impl FromStr for Direction2D {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Self::U),
            "UR" => Ok(Self::UR),
            "R" => Ok(Self::R),
            "DR" => Ok(Self::DR),
            "D" => Ok(Self::D),
            "DL" => Ok(Self::DL),
            "L" => Ok(Self::L),
            "UL" => Ok(Self::UL),
            _ => Err(()),
        }
    }
}

impl Sub for Point2D {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self::new(self.x - other.x, self.y - other.y)
    }
}

impl Add for Point2D {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self::new(self.x + other.x, self.y + other.y)
    }
}

impl Point2D {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x: x, y: y }
    }

    pub fn origin() -> Self {
        Self::new(0, 0)
    }

    pub fn step_2d_unchecked(self, dir: Direction2D) -> Self {
        match dir {
            Direction2D::U => Self::new(self.x, self.y - 1),
            Direction2D::UR => Self::new(self.x + 1, self.y - 1),
            Direction2D::R => Self::new(self.x + 1, self.y),
            Direction2D::DR => Self::new(self.x + 1, self.y + 1),
            Direction2D::D => Self::new(self.x, self.y + 1),
            Direction2D::DL => Self::new(self.x - 1, self.y + 1),
            Direction2D::L => Self::new(self.x - 1, self.y),
            Direction2D::UL => Self::new(self.x - 1, self.y - 1),
        }
    }

    pub fn step_2d(self, begin: Self, end: Self, dir: Direction2D) -> Option<Self> {
        //println!("{:?} {:?}, {:?}", dir, (w, h), (x, y));
        let unchecked = Some(self.step_2d_unchecked(dir));
        match dir {
            Direction2D::U if self.y > begin.y => unchecked,
            Direction2D::UR if self.x < end.x - 1 && self.y > begin.y => unchecked,
            Direction2D::R if self.x < end.x - 1 => unchecked,
            Direction2D::DR if self.x < end.x - 1 && self.y < end.y - 1 => unchecked,
            Direction2D::D if self.y < end.y - 1 => unchecked,
            Direction2D::DL if self.x > begin.x && self.y < end.y - 1 => unchecked,
            Direction2D::L if self.x > begin.x => unchecked,
            Direction2D::UL if self.x > begin.x && self.y > begin.y => unchecked,
            _ => None,
        }
    }

    pub fn is_neighbor(self, other: Self) -> bool {
        std::cmp::max(num::abs(self.x - other.x), num::abs(self.y - other.y)) < 2
    }
}
