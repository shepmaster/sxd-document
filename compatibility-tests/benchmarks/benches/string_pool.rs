use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use sxd_document::__internal::StringPool;

fn single_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_string");

    let original = "hello";

    group.throughput(Throughput::Bytes(original.len() as u64));
    group.bench_function("single_string", |b| {
        let pool = StringPool::new();
        b.iter(|| pool.intern(original));
    });

    group.finish();
}

fn many_unique_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("many_unique_string");

    let strings: Vec<_> = (0..1000).map(|i| format!("str{i}str")).collect();
    let total_len = strings.iter().map(|s| s.len()).sum::<usize>();

    group.throughput(Throughput::Bytes(total_len as u64));
    group.bench_function("many_unique_string", |b| {
        let pool = StringPool::new();

        b.iter(|| {
            for s in &strings {
                pool.intern(s);
            }
        });
    });

    group.finish();
}

fn many_repeated_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("many_repeated_string");

    let strings: Vec<_> = (0..1000).map(|i| format!("str{}str", i % 100)).collect();
    let total_len = strings.iter().map(|s| s.len()).sum::<usize>();

    group.throughput(Throughput::Bytes(total_len as u64));
    group.bench_function("many_unique_string", |b| {
        let pool = StringPool::new();

        b.iter(|| {
            for s in &strings {
                pool.intern(s);
            }
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    single_string,
    many_unique_string,
    many_repeated_string,
);
criterion_main!(benches);
