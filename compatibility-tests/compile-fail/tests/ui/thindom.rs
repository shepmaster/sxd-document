use sxd_document::Package;

fn cannot_mutate_connections_while_iterating_over_root_children() {
    let package = Package::new();
    let (s, mut c) = package.as_thin_document();

    let alpha = s.create_element("alpha");

    for _ in c.root_children() {
        c.append_root_child(alpha);
    }
}

fn cannot_mutate_connections_while_iterating_over_element_children() {
    let package = Package::new();
    let (s, mut c) = package.as_thin_document();

    let alpha = s.create_element("alpha");
    let beta = s.create_element("beta");

    for _ in c.element_children(alpha) {
        c.append_element_child(alpha, beta);
    }
}

fn cannot_mutate_connections_while_iterating_over_siblings() {
    let package = Package::new();
    let (s, mut c) = package.as_thin_document();

    let alpha = s.create_element("alpha");
    let beta = s.create_element("beta");

    for _ in c.element_preceding_siblings(alpha) {
        c.append_element_child(alpha, beta);
    }
}

fn nodes_cannot_live_outside_of_the_document() {
    let package = Package::new();

    let _node = {
        let (s, _) = package.as_thin_document();

        s.create_element("hello")
    };
}

fn main() {}
