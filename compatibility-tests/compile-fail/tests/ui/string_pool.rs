use sxd_document::__internal::StringPool;

fn string_cannot_outlive_the_pool() {
    let _s = {
        let pool = StringPool::new();
        pool.intern("hello")
    };
}

fn main() {}
