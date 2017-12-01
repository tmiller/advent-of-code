#![feature(io)]
use std::io::Read;
use std::fs::File;

fn main() {
    let input = File::open("inputs/part-one.txt").unwrap();
    let numbers = read_to_num(input);
    let p1_result = solve_part_one(numbers);

    println!("Part One: {:?}", p1_result);
}

fn solve_part_one(input: Vec<i32>) -> Option<i32> {
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

fn append_first_item(input: Vec<i32>) -> Option<Vec<i32>> {
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
    fn p1_it_returns_none_with_empty_vector() {
        let input = vec![];
        let actual = solve_part_one(input);
        let expected = None;
        assert_eq!(expected, actual);
    }

    #[test]
    fn p1_it_solves_1122() {
        let input = vec![1, 1, 2, 2];
        let actual = solve_part_one(input);
        let expected = Some(3);
        assert_eq!(expected, actual);
    }

    #[test]
    fn p1_it_solves_1121() {
        let input = vec![1, 1, 1, 1];
        let actual = solve_part_one(input);
        let expected = Some(4);
        assert_eq!(expected, actual);
    }

    #[test]
    fn p1_it_solves_1234() {
        let input = vec![1, 2, 3, 4];
        let actual = solve_part_one(input);
        let expected = Some(0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn p1_it_solves_91212129() {
        let input = vec![9, 1, 2, 1, 2, 1, 2, 9];
        let actual = solve_part_one(input);
        let expected = Some(9);
        assert_eq!(expected, actual);
    }

    #[test]
    fn append_first_item_returns_none_with_empty_vector() {
        let actual = append_first_item(Vec::new());
        let expected = None;
        assert_eq!(expected, actual);
    }

    #[test]
    fn append_first_item_adds_first_item_to_end() {
        let input = vec![1, 2, 3];
        let actual = append_first_item(input);
        let expected = Some(vec![1, 2, 3, 1]);
        assert_eq!(expected, actual);
    }

    #[test]
    fn read_to_num_returns_empty_list() {
        let readable = Cursor::new([]);
        let actual = read_to_num(readable);
        let expected: Vec<i32> = Vec::new();
        assert_eq!(expected, actual);
    }

    #[test]
    fn read_to_num_reads_characters_to_numbers() {
        let input = String::from("12x34");
        let readable = Cursor::new(input.as_bytes());
        let actual = read_to_num(readable);
        let expected = vec![1, 2, 3, 4];
        assert_eq!(expected, actual);
    }
}
