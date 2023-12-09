pub fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub fn lcd(a: i64, b: i64) -> i64 {
    a * b / gcd(a, b)
}
