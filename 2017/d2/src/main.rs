#![feature(slice_patterns)]

extern crate itertools;

use std::fs::File;
use std::io::{Seek, SeekFrom, BufRead, BufReader};
use itertools::Itertools;
use std::fmt::Debug;

fn main() {
    let file = File::open("./input.txt").unwrap();
    let reader = &mut BufReader::new(&file);
    println!("Part One: {:?}", solve_part_one(reader));
    println!("Part Two: {:?}", solve_part_two(reader));

}

fn solve_part_one<T: BufRead + Seek>(reader: &mut T) -> i32 {
    let result = reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            let items: Vec<i32> = line.split('\t')
                .filter_map(|num| num.parse::<i32>().ok())
                .collect();
            let min = items.iter().min().unwrap();
            let max = items.iter().max().unwrap();
            max - min
        })
        .sum::<i32>();
    reader.seek(SeekFrom::Start(0)).unwrap();
    result
}

fn solve_part_two<T: BufRead + Seek + Debug>(reader: &mut T) -> i32 {
    let result: i32= reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            let mut items = line
                .split('\t')
                .filter_map(|num| num.parse::<i32>().ok())
                .collect::<Vec<i32>>().clone();
            items.sort();
            items.reverse();
            items
                .iter()
                .combinations(2)
                .find(|comb| match comb.as_slice() {
                    &[x, y] => (x % y) == 0,
                    _ => false,
                })
                .map(|comb| match comb.as_slice() {
                    &[x, y]=> x / y,
                    _ => 0
                })
        })
        .filter_map(|x| x)
        .sum();
    reader.seek(SeekFrom::Start(0)).unwrap();
    result
}
