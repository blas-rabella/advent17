use std::io;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let numbers: Vec<u32> =
        input.trim().chars().map(|c| c.to_digit(10).unwrap()).collect();
    let n = numbers.len();
    let mut result = 0;
    for i in 0..n {
        // let j = (i+1) % n;
        let j = (i+(n/2)) % n;
        result = result + f(numbers[i], numbers[j]);
    }
    println!("result: {}", result)
}

fn f(a : u32, b : u32) -> u32{
    if a == b {
        return a;
    } else {
        return 0;
    }
}
