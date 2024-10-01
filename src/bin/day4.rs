use std::{
    collections::HashMap,
    io::{stdin, Read},
};

static REQUIRED_FIELDS: [&str; 7] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

static VALID_EYE_COLORS: [&str; 7] = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

#[derive(Debug, PartialEq, Eq)]
struct Document(HashMap<String, String>);

fn is_num_valid(s: &str, min: u32, max: u32) -> bool {
    s.parse().map_or(false, |n| min <= n && n <= max)
}

impl Document {
    fn contains_required_fields(&self) -> bool {
        REQUIRED_FIELDS
            .iter()
            .all(|&field| self.0.contains_key(field))
    }

    fn check_num_field(&self, field: &str, min: u32, max: u32) -> bool {
        self.0
            .get(field)
            .map_or(false, |s| is_num_valid(s, min, max))
    }

    fn is_valid(&self) -> bool {
        true && self.check_num_field("byr", 1920, 2002)
            && self.check_num_field("iyr", 2010, 2020)
            && self.check_num_field("eyr", 2020, 2030)
            && self
                .0
                .get("hgt")
                .map_or(false, |s| parse::height_is_valid(s))
            && self
                .0
                .get("hcl")
                .map_or(false, |s| parse::hair_color_is_valid(&s))
            && self
                .0
                .get("ecl")
                .map_or(false, |ecl| VALID_EYE_COLORS.contains(&ecl.as_str()))
            && self
                .0
                .get("pid")
                .map_or(false, |p| p.len() == 9 && p.chars().all(|c| c.is_numeric()))
    }
}

impl FromIterator<(String, String)> for Document {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

fn part1(input: &Vec<Document>) {
    let valid_count = input
        .iter()
        .filter(|d| d.contains_required_fields())
        .count();
    println!("Part1: {}", valid_count);
}

fn part2(input: &Vec<Document>) {
    let valid_count = input.iter().filter(|d| d.is_valid()).count();
    println!("Part2: {}", valid_count);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let (_, input) = parse::full_input(&raw_input).expect("Failed to parse input");
    part1(&input);
    part2(&input);
}

mod parse {
    use nom::{
        bytes::complete::{tag, take_while1},
        character::complete::{alphanumeric1, char, digit1, multispace0, newline, satisfy, space1},
        combinator::{complete, map_res, value},
        multi::{count, many1, separated_list1},
        sequence::{preceded, terminated, tuple, Tuple},
        AsChar, Parser,
    };

    use crate::Document;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn field(input: &str) -> IResult<(&str, &str)> {
        let (input, (key, _, value)) = (
            alphanumeric1,
            char(':'),
            take_while1(|c: char| !c.is_whitespace()),
        )
            .parse(input)?;
        Ok((input, (key, value)))
    }

    fn document(input: &str) -> IResult<Document> {
        separated_list1(space1.map(|_| ()).or(newline.map(|_| ())), field)
            .map(|fields| {
                Document::from_iter(fields.iter().map(|(k, v)| (k.to_string(), v.to_string())))
            })
            .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> IResult<Vec<Document>> {
        complete(terminated(
            separated_list1(many1(newline), document),
            multispace0,
        ))(input)
    }

    pub(crate) fn height_is_valid(input: &str) -> bool {
        #[derive(Clone, Copy)]
        enum Unit {
            Cm,
            Inch,
        }

        let number = map_res(digit1::<&str, ()>, |s: &str| s.parse::<u32>());
        let unit = value(Unit::Cm, tag("cm")).or(value(Unit::Inch, tag("in")));
        complete(tuple((number, unit)))(input).map_or(false, |(_, (n, u))| match u {
            Unit::Cm => 150 <= n && n <= 193,
            Unit::Inch => 59 <= n && n <= 76,
        })
    }

    pub(crate) fn hair_color_is_valid(input: &str) -> bool {
        let hex_digit = satisfy(|c| c.is_hex_digit() && !c.is_uppercase());
        complete(preceded(char::<&str, ()>('#'), count(hex_digit, 6)))(input).is_ok()
    }

    #[cfg(test)]
    mod test {
        use std::collections::HashMap;

        use super::*;

        #[test]
        fn test_field() {
            assert_eq!(field("abc:#!abc123"), Ok(("", ("abc", "#!abc123"))));
        }

        #[test]
        fn test_document_single() {
            let mut m = HashMap::new();
            m.insert("abc".to_string(), "#!abc123".to_string());
            assert_eq!(document("abc:#!abc123"), Ok(("", Document(m))));
        }

        #[test]
        fn test_document_sep_space() {
            let mut m = HashMap::new();
            m.insert("abc".to_string(), "123".to_string());
            m.insert("def".to_string(), "456".to_string());
            assert_eq!(document("abc:123 def:456"), Ok(("", Document(m))));
        }

        #[test]
        fn test_document_sep_newline() {
            let mut m = HashMap::new();
            m.insert("abc".to_string(), "123".to_string());
            m.insert("def".to_string(), "456".to_string());
            assert_eq!(document("abc:123\ndef:456"), Ok(("", Document(m))));
        }

        #[test]
        fn test_full_input() {
            let result = full_input(
                "\
                ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                byr:1937 iyr:2017 cid:147 hgt:183cm\n\
                \n\
                iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                hcl:#cfa07d byr:1929
                ",
            );
            let (input, docs) = result.unwrap();
            assert_eq!(input, "");
            assert_eq!(docs.len(), 2);
        }
    }
}
