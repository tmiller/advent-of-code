#![feature(io)]
use std::io::Read;
use std::fs::File;

fn main() {
    let input = File::open("inputs/part-one.txt").unwrap();
    let numbers = &read_to_num(input);
    if let Some(r) = solve_part_one(numbers) { println!("P1: {}", r) };
    if let Some(r) = solve_part_two(numbers) { println!("P2: {}", r) };
}

fn solve_part_one(input: &Vec<i32>) -> Option<i32> {
    let items = append_first_item(input)?;

    let mut prev: Option<i32> = None;
    let mut acc: i32 = 0;

    for item in items {
        if let Some(prev) = prev {
            if item == prev {
                acc += item
            }
        }
        prev = Some(item);
    }

    Some(acc)
}

fn solve_part_two(input: &Vec<i32>) -> Option<i32> {
    if input.is_empty() || input.len() % 2 != 0 {
        return None;
    };

    let mid = input.len() / 2;
    let mut acc = 0;

    for (idx, item) in input.iter().enumerate() {
        if *item == input[(idx + mid) % input.len()] {
            acc += item
        }
    }

    Some(acc)
}

fn append_first_item(input: &Vec<i32>) -> Option<Vec<i32>> {
    let first = *input.get(0)?;
    let mut items = input.clone();
    items.push(first);
    Some(items)
}

fn read_to_num<T: Read>(input: T) -> Vec<i32> {
    input
        .chars()
        .filter_map(Result::ok)
        .filter_map(|x| x.to_string().parse().ok())
        .collect::<Vec<i32>>()
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn p2_it_returns_none_with_empty_vector() {
        assert_eq!(None, solve_part_two(&vec![]));
    }

    #[test]
    fn p2_it_returns_none_with_odd_input() {
        assert_eq!(None, solve_part_two(&vec![1]));
    }

    #[test]
    fn p2_it_solves_1212() {
        assert_eq!(Some(6), solve_part_two(&vec![1, 2, 1, 2]));
    }

    #[test]
    fn p2_it_solves_1221() {
        assert_eq!(Some(0), solve_part_two(&vec![1, 2, 2, 1]));
    }

    #[test]
    fn p2_it_solves_123425() {
        assert_eq!(Some(4), solve_part_two(&vec![1, 2, 3, 4, 2, 5]));
    }

    #[test]
    fn p2_it_solves_123123() {
        assert_eq!(Some(12), solve_part_two(&vec![1, 2, 3, 1, 2, 3]));
    }

    #[test]
    fn p2_it_solves_12131415() {
        assert_eq!(Some(4), solve_part_two(&vec![1, 2, 1, 3, 1, 4, 1, 5]));
    }

    #[test]
    fn p1_it_returns_none_with_empty_vector() {
        assert_eq!(None, solve_part_one(&vec![]));
    }

    #[test]
    fn p1_it_solves_1122() {
        assert_eq!(Some(3), solve_part_one(&vec![1, 1, 2, 2]));
    }

    #[test]
    fn p1_it_solves_1121() {
        assert_eq!(Some(4), solve_part_one(&vec![1, 1, 1, 1]));
    }

    #[test]
    fn p1_it_solves_1234() {
        assert_eq!(Some(0), solve_part_one(&vec![1, 2, 3, 4]));
    }

    #[test]
    fn p1_it_solves_91212129() {
        assert_eq!(Some(9), solve_part_one(&vec![9, 1, 2, 1, 2, 1, 2, 9]));
    }

    #[test]
    fn append_first_item_returns_none_with_empty_vector() {
        assert_eq!(None, append_first_item(&Vec::new()));
    }

    #[test]
    fn append_first_item_adds_first_item_to_end() {
        assert_eq!(Some(vec![1, 2, 3, 1]), append_first_item(&vec![1, 2, 3]));
    }

    #[test]
    fn read_to_num_returns_empty_list() {
        assert_eq!(Vec::<i32>::new(), read_to_num(Cursor::new([])));
    }

    #[test]
    fn read_to_num_reads_characters_to_numbers() {
        let actual = read_to_num(Cursor::new("12x34".as_bytes()));
        assert_eq!(vec![1, 2, 3, 4], actual);
    }
}
