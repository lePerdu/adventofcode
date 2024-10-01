use std::io::{stdin, Read};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Operator {
    Add,
    Mul,
}

impl Operator {
    pub fn apply(&self, lhs: u64, rhs: u64) -> u64 {
        match self {
            Operator::Add => lhs + rhs,
            Operator::Mul => lhs * rhs,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Num(u64),
    Op {
        op: Operator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    pub fn num(n: u64) -> Box<Self> {
        Box::new(Expr::Num(n))
    }

    pub fn op(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Self> {
        Box::new(Expr::Op { op, lhs, rhs })
    }

    pub fn eval(&self) -> u64 {
        match self {
            Expr::Num(n) => *n,
            Expr::Op { op, lhs, rhs } => op.apply(lhs.eval(), rhs.eval()),
        }
    }
}

type Input = Vec<Box<Expr>>;

fn part1(raw_input: &str) {
    let (_, input) = parse::full_input1(raw_input).expect("Failed to parse input");
    let ans: u64 = input.iter().map(|e| e.eval()).sum();
    println!("Part1: {}", ans);
}

fn part2(raw_input: &str) {
    let (_, input) = parse::full_input2(raw_input).expect("Failed to parse input");
    let ans: u64 = input.iter().map(|e| e.eval()).sum();
    println!("Part2: {}", ans);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    part1(&raw_input);
    part2(&raw_input);
}

mod parse {
    use super::*;

    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{multispace0, space0, u64},
        combinator::{all_consuming, value},
        error::ParseError,
        multi::many1,
        sequence::{delimited, terminated},
        Parser,
    };

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn num(input: &str) -> IResult<Box<Expr>> {
        u64.map(Expr::num).parse(input)
    }

    fn with_spaces<'a, O, E: ParseError<&'a str>>(
        p: impl Parser<&'a str, O, E>,
    ) -> impl Parser<&'a str, O, E> {
        delimited(space0, p, space0)
    }

    fn op(input: &str) -> IResult<Operator> {
        with_spaces(alt((
            value(Operator::Add, tag("+")),
            value(Operator::Mul, tag("*")),
        )))
        .parse(input)
    }

    fn expr_with_prec<'a, P>(
        get_prec: P,
        current_prec: u8,
    ) -> impl Parser<&'a str, Box<Expr>, nom::error::Error<&'a str>>
    where
        P: Fn(Operator) -> u8 + Copy,
    {
        move |input| {
            let parens_prec = delimited(tag("("), expr_with_prec(get_prec, 0), tag(")"));
            let mut atom_prec = alt((num, parens_prec));

            let (rest, lhs) = atom_prec.parse(input)?;
            let mut input = rest;
            let mut output = lhs;
            loop {
                match op(input) {
                    Ok((rest_after_op, operator)) => {
                        let op_prec = get_prec(operator);
                        if op_prec >= current_prec {
                            let (rest_after_rhs, rhs) =
                                expr_with_prec(get_prec, op_prec + 1).parse(rest_after_op)?;
                            input = rest_after_rhs;
                            output = Expr::op(operator, output, rhs);
                        } else {
                            break;
                        }
                    }
                    _ => break,
                }
            }

            Ok((input, output))
        }
    }

    fn expr_even_prec(input: &str) -> IResult<Box<Expr>> {
        expr_with_prec(|_op| 1, 0).parse(input)
    }

    fn expr_add_over_mul(input: &str) -> IResult<Box<Expr>> {
        expr_with_prec(
            |op| match op {
                Operator::Add => 2,
                Operator::Mul => 1,
            },
            0,
        )
        .parse(input)
    }

    pub(crate) fn full_input1(input: &str) -> IResult<Input> {
        all_consuming(many1(terminated(expr_even_prec, multispace0)))(input)
    }

    pub(crate) fn full_input2(input: &str) -> IResult<Input> {
        all_consuming(many1(terminated(expr_add_over_mul, multispace0)))(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn number() {
            assert_eq!(expr_even_prec("12"), Ok(("", Expr::num(12))));
        }

        #[test]
        fn simple_expr() {
            assert_eq!(
                expr_even_prec("2 + 3"),
                Ok(("", Expr::op(Operator::Add, Expr::num(2), Expr::num(3),)))
            );
        }

        #[test]
        fn simple_expr_in_parens() {
            assert_eq!(
                expr_even_prec("(2 + 3)"),
                Ok(("", Expr::op(Operator::Add, Expr::num(2), Expr::num(3),)))
            );
        }

        #[test]
        fn three_part_expr() {
            assert_eq!(
                expr_even_prec("1 + 2 * 3"),
                Ok((
                    "",
                    Expr::op(
                        Operator::Mul,
                        Expr::op(Operator::Add, Expr::num(1), Expr::num(2)),
                        Expr::num(3),
                    )
                ))
            );
        }

        #[test]
        fn parens_at_start() {
            assert_eq!(
                expr_even_prec("(2 + 3) * 4 + 5"),
                Ok((
                    "",
                    Expr::op(
                        Operator::Add,
                        Expr::op(
                            Operator::Mul,
                            Expr::op(Operator::Add, Expr::num(2), Expr::num(3)),
                            Expr::num(4)
                        ),
                        Expr::num(5),
                    )
                ))
            )
        }

        #[test]
        fn expr_with_parens_at_end() {
            assert_eq!(
                expr_even_prec("2 + 3 * (4 + 5)"),
                Ok((
                    "",
                    Expr::op(
                        Operator::Mul,
                        Expr::op(Operator::Add, Expr::num(2), Expr::num(3)),
                        Expr::op(Operator::Add, Expr::num(4), Expr::num(5)),
                    ),
                ))
            );
        }
    }
}

#[cfg(test)]
mod test {}
