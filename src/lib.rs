#![feature(iterator_try_collect)]

/// 2D integer coordinate type and helpers
pub mod coord {
    use std::{
        fmt::Debug,
        ops::{Add, Mul, Sub},
    };

    use ndarray::{Ix2, NdIndex};
    use num::{CheckedSub, Integer, Signed, ToPrimitive, Unsigned};

    /// Generic coordinate structure
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Coord<T> {
        pub row: T,
        pub col: T,
    }

    /// Unsigned coordinate
    pub type UCoord = Coord<usize>;

    /// Signed coordinate
    pub type ICoord = Coord<isize>;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Direction {
        R,
        RU,
        U,
        LU,
        L,
        LD,
        D,
        RD,
    }

    pub static ALL_DIR4: [Direction; 4] = {
        use Direction::*;
        [R, U, L, D]
    };

    pub static ALL_DIR8: [Direction; 8] = {
        use Direction::*;
        [R, RU, U, LU, L, LD, D, RD]
    };

    impl<T> Coord<T> {
        pub const fn new(row: T, col: T) -> Self {
            Self { row, col }
        }
    }

    impl<T: PartialOrd> Coord<T> {
        pub fn within(&self, bound: &Self) -> bool {
            self.row < bound.row && self.col < bound.col
        }
    }

    impl<T> From<(T, T)> for Coord<T> {
        fn from((row, col): (T, T)) -> Self {
            Self::new(row, col)
        }
    }

    impl<T> From<Coord<T>> for (T, T) {
        fn from(coord: Coord<T>) -> Self {
            (coord.row, coord.col)
        }
    }

    impl From<UCoord> for ICoord {
        fn from(value: UCoord) -> Self {
            Self::new(value.row as isize, value.col as isize)
        }
    }

    impl TryFrom<ICoord> for UCoord {
        type Error = <usize as TryFrom<isize>>::Error;

        fn try_from(value: ICoord) -> Result<Self, Self::Error> {
            Ok(Self::new(value.row.try_into()?, value.col.try_into()?))
        }
    }

    impl ICoord {
        pub fn manhattan(&self, other: &Self) -> usize {
            self.row.abs_diff(other.row) + self.col.abs_diff(other.col)
        }
    }

    impl UCoord {
        pub fn manhattan(&self, other: &Self) -> usize {
            self.row.abs_diff(other.row) + self.col.abs_diff(other.col)
        }
    }

    impl<T: Integer + Signed + Copy> Coord<T> {
        /// Iterator over the 4 adjacent cells, up, down, left, and right
        pub fn iter_adj4<'a>(&'a self) -> impl Iterator<Item = Self> + 'a {
            [
                Coord::new(T::zero(), T::one()),
                Coord::new(-T::one(), T::zero()),
                Coord::new(T::zero(), -T::one()),
                Coord::new(T::one(), T::zero()),
            ]
            .into_iter()
            .map(|delta| *self + delta)
        }

        /// Iterator over all 8 adjacent cells, including diagonals
        pub fn iter_adj8<'a>(&'a self) -> impl Iterator<Item = Self> + 'a {
            [
                Coord::new(T::zero(), T::one()),
                Coord::new(-T::one(), T::one()),
                Coord::new(-T::one(), T::zero()),
                Coord::new(-T::one(), -T::one()),
                Coord::new(T::zero(), -T::one()),
                Coord::new(T::one(), -T::one()),
                Coord::new(T::one(), T::zero()),
                Coord::new(T::one(), T::one()),
            ]
            .into_iter()
            .map(|delta| *self + delta)
        }
    }

    impl<T: Integer + Unsigned + Copy + CheckedSub> Coord<T> {
        #[inline]
        fn try_apply(
            &self,
            map_row: impl FnOnce(T) -> Option<T>,
            map_col: impl FnOnce(T) -> Option<T>,
        ) -> Option<Self> {
            Some(Self::new(map_row(self.row)?, map_col(self.col)?))
        }

        pub fn move_dir(&self, dir: Direction, upper_bound: impl Into<Coord<T>>) -> Option<Self> {
            let res = match dir {
                Direction::R => self.try_apply(|r| Some(r), |c| Some(c + T::one())),
                Direction::RU => {
                    self.try_apply(|r| r.checked_sub(&T::one()), |c| Some(c + T::one()))
                }
                Direction::U => self.try_apply(|r| r.checked_sub(&T::one()), |c| Some(c)),
                Direction::LU => {
                    self.try_apply(|r| r.checked_sub(&T::one()), |c| c.checked_sub(&T::one()))
                }
                Direction::L => self.try_apply(|r| Some(r), |c| c.checked_sub(&T::one())),
                Direction::LD => {
                    self.try_apply(|r| Some(r + T::one()), |c| c.checked_sub(&T::one()))
                }
                Direction::D => self.try_apply(|r| Some(r + T::one()), |c| Some(c)),
                Direction::RD => self.try_apply(|r| Some(r + T::one()), |c| Some(c + T::one())),
            }?;

            if res.within(&upper_bound.into()) {
                Some(res)
            } else {
                None
            }
        }

        pub fn scan_dir<'a>(
            &'a self,
            dir: Direction,
            upper_bound: impl Into<Self>,
        ) -> impl Iterator<Item = Self> {
            let upper_bound: Self = upper_bound.into();
            // skip(1) to not return initial value
            std::iter::successors(Some(*self), move |c| c.move_dir(dir, upper_bound)).skip(1)
        }

        /// Iterator over the 4 adjacent cells, up, down, left, and right
        pub fn iter_adj4_bounded<'a>(
            &'a self,
            upper_bound: impl Into<Self>,
        ) -> impl Iterator<Item = Self> + 'a {
            let upper_bound: Self = upper_bound.into();
            ALL_DIR4
                .iter()
                .filter_map(move |d| self.move_dir(*d, upper_bound))
        }

        /// Iterator over all 8 adjacent cells, including diagonals
        pub fn iter_adj8_bounded<'a>(
            &'a self,
            upper_bound: impl Into<Self>,
        ) -> impl Iterator<Item = Self> + 'a {
            let upper_bound: Self = upper_bound.into();
            ALL_DIR8
                .iter()
                .filter_map(move |d| self.move_dir(*d, upper_bound))
        }
    }

    impl<T: Integer> Add<Self> for Coord<T> {
        type Output = Self;

        fn add(self, rhs: Self) -> Self::Output {
            Coord::new(self.col + rhs.col, self.row + rhs.row)
        }
    }

    impl<T: Integer> Sub<Self> for Coord<T> {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            Coord::new(self.col - rhs.col, self.row - rhs.row)
        }
    }

    impl<T: Integer + CheckedSub> CheckedSub for Coord<T> {
        fn checked_sub(&self, rhs: &Self) -> Option<Self::Output> {
            Some(Coord::new(
                self.col.checked_sub(&rhs.col)?,
                self.row.checked_sub(&rhs.row)?,
            ))
        }
    }

    impl<T: Integer + Copy> Mul<T> for Coord<T> {
        type Output = Self;

        fn mul(self, rhs: T) -> Self {
            Self::new(self.row * rhs, self.col * rhs)
        }
    }

    unsafe impl NdIndex<Ix2> for UCoord {
        #[inline]
        fn index_checked(&self, dim: &Ix2, strides: &Ix2) -> Option<isize> {
            (self.row.to_usize()?, self.col.to_usize()?).index_checked(dim, strides)
        }

        #[inline]
        fn index_unchecked(&self, strides: &Ix2) -> isize {
            (self.row.to_usize().unwrap(), self.col.to_usize().unwrap()).index_unchecked(strides)
        }
    }
}

pub mod grid {
    use ndarray::{Array1, Array2};

    pub type Grid<T> = Array2<T>;

    fn parse_row<T>(row: &str, parse_cell: impl Fn(char) -> Option<T>) -> Option<Vec<T>> {
        row.trim_end().chars().map(parse_cell).try_collect()
    }

    pub fn parse<'a, T>(input: &str, parse_cell: impl Fn(char) -> Option<T>) -> Option<Grid<T>> {
        let vec = input
            .lines()
            .map(|s| parse_row(&s, &parse_cell))
            .try_collect::<Vec<_>>()?;
        let shape = (vec.len(), vec[0].len());
        // Ensure all rows are the same length
        if !vec.iter().all(|v| v.len() == shape.1) {
            return None;
        }

        let flat = Array1::from_iter(vec.into_iter().flatten());
        flat.into_shape(shape).map_or(None, Some)
    }
}
