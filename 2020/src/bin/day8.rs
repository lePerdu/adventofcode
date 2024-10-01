#![feature(iterator_try_collect)]

use std::io::stdin;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InstrType {
    Nop,
    Acc,
    Jmp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Instr {
    ty: InstrType,
    arg: i32,
}

type Accumulator = i32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProgResult {
    Loop { pc: usize, acc: Accumulator },
    OutOfBounds { pc: usize, acc: Accumulator },
    Ends { acc: Accumulator },
}

trait Program {
    fn len(&self) -> usize;
    fn get_instr(&self, index: usize) -> Option<&Instr>;
}

impl Program for Vec<Instr> {
    fn len(&self) -> usize {
        self.len()
    }

    fn get_instr(&self, index: usize) -> Option<&Instr> {
        self.get(index)
    }
}

struct ChangedProgram<'a, T> {
    base_program: &'a T,
    change_index: usize,
    change_instr: Instr,
}

impl<'a, T: Program> ChangedProgram<'a, T> {
    fn with_new_type(base: &'a T, change_index: usize, instr_type: InstrType) -> Option<Self> {
        base.get_instr(change_index).map(|existing_instr| Self {
            base_program: base,
            change_index,
            change_instr: Instr {
                ty: instr_type,
                arg: existing_instr.arg,
            },
        })
    }
}

impl<'a, T: Program> Program for ChangedProgram<'a, T> {
    fn len(&self) -> usize {
        return self.base_program.len();
    }

    fn get_instr(&self, index: usize) -> Option<&Instr> {
        if index == self.change_index {
            Some(&self.change_instr)
        } else {
            self.base_program.get_instr(index)
        }
    }
}

fn check_termination<P: Program>(prog: &P) -> ProgResult {
    let mut visited = vec![false; prog.len()];

    let mut pc = 0;
    let mut acc = 0;
    loop {
        match visited.get(pc) {
            Some(true) => return ProgResult::Loop { pc, acc },
            Some(false) => visited[pc] = true,
            None => {
                return if pc == prog.len() {
                    ProgResult::Ends { acc }
                } else {
                    ProgResult::OutOfBounds { pc, acc }
                }
            }
        }

        // Length already checked with visited
        let instr = prog.get_instr(pc).unwrap();
        match instr.ty {
            InstrType::Nop => pc += 1,
            InstrType::Acc => {
                acc += instr.arg;
                pc += 1;
            }
            InstrType::Jmp => pc = (pc as i32 + instr.arg) as usize,
        }
    }
}

fn termination_with_swap<P: Program>(prog: &P, swap_index: usize) -> Option<ProgResult> {
    prog.get_instr(swap_index)
        .and_then(|instr| match instr.ty {
            InstrType::Nop => ChangedProgram::with_new_type(prog, swap_index, InstrType::Jmp),
            InstrType::Jmp => ChangedProgram::with_new_type(prog, swap_index, InstrType::Nop),
            _ => None,
        })
        .map(|changed| check_termination(&changed))
}

fn part1<P: Program>(prog: &P) {
    print!("Part1: ");
    match check_termination(prog) {
        ProgResult::Loop { acc, .. } => println!("{}", acc),
        not_loop => println!("Did not loop: {:?}", not_loop),
    }
}

fn part2<P: Program>(prog: &P) {
    let ans = (0..(prog.len() - 1))
        .filter_map(|index| termination_with_swap(prog, index))
        .find_map(|res| match res {
            ProgResult::Ends { acc } => Some(acc),
            _ => None,
        })
        .expect("None terminate");
    println!("Part2: {}", ans);
}

fn main() {
    let program = stdin()
        .lines()
        .map(|l| l.expect("Failed to read input"))
        .map(|l| parse::instr(&l))
        .try_collect::<Vec<_>>()
        .expect("Failed to parse input");
    part1(&program);
    part2(&program);
}

mod parse {
    use crate::*;

    fn instr_type(input: &str) -> Option<InstrType> {
        match input {
            "nop" => Some(InstrType::Nop),
            "acc" => Some(InstrType::Acc),
            "jmp" => Some(InstrType::Jmp),
            _ => None,
        }
    }

    pub(crate) fn instr(input: &str) -> Option<Instr> {
        input.split_once(char::is_whitespace).and_then(|(ty, arg)| {
            let ty = instr_type(ty)?;
            let arg = arg.parse::<i32>().ok()?;
            Some(Instr { ty, arg })
        })
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_instr() {
            assert_eq!(
                instr("nop +1"),
                Some(Instr {
                    ty: InstrType::Nop,
                    arg: 1
                })
            );
            assert_eq!(
                instr("acc -2"),
                Some(Instr {
                    ty: InstrType::Acc,
                    arg: -2
                })
            );
        }
    }
}
