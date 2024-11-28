use std::{
    fs::File,
    io::{BufWriter, Write},
    path::Path,
    time::{Duration, Instant},
};

use crate::{
    search::{InitialPosition, SearchResult},
    trans_table::AB,
    ui::render_sparkline,
    Card, CardSet, PlayOfCards, PlayState,
};

pub(crate) trait Observer {
    fn node_enter<PoC: PlayOfCards>(&mut self, target: u8, state: &PlayState<PoC>);
    fn node_exit<PoC: PlayOfCards>(
        &mut self,
        target: u8,
        state: &PlayState<PoC>,
        result: &SearchResult,
    );

    fn child_enter(&mut self, card: Card);
    fn child_exit(&mut self, card: Card);

    fn update_table<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB);

    fn guess_iter(&mut self, target: u8);
    fn guess_iter_done(&mut self, target: u8);
    fn max_tricks_cutoff(&mut self);
    fn quick_tricks_cutoff(&mut self);
    fn search_cutoff(&mut self);
    fn search_finished(&mut self, init: &InitialPosition);
    fn search_started(&mut self);
    fn tt_hit<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB);
    fn tt_lower_cutoff(&mut self);
    fn tt_upper_cutoff(&mut self);
}

#[derive(Debug, Default)]
pub struct Empty {}

impl Observer for Empty {
    fn node_enter<PoC: PlayOfCards>(&mut self, _target: u8, _state: &PlayState<PoC>) {}
    fn node_exit<PoC: PlayOfCards>(
        &mut self,
        _target: u8,
        _state: &PlayState<PoC>,
        _result: &SearchResult,
    ) {
    }

    fn tt_lower_cutoff(&mut self) {}

    fn tt_upper_cutoff(&mut self) {}

    fn guess_iter(&mut self, _target: u8) {}
    fn guess_iter_done(&mut self, _target: u8) {}

    fn max_tricks_cutoff(&mut self) {}

    fn quick_tricks_cutoff(&mut self) {}

    fn search_cutoff(&mut self) {}

    fn search_finished(&mut self, _init: &InitialPosition) {}

    fn search_started(&mut self) {}

    fn tt_hit<PoC: PlayOfCards>(&mut self, _state: &PlayState<PoC>, _rel_mask: CardSet, _ab: AB) {}

    fn child_enter(&mut self, _card: Card) {}
    fn child_exit(&mut self, _card: Card) {}

    fn update_table<PoC: PlayOfCards>(
        &mut self,
        _state: &PlayState<PoC>,
        _rel_mask: CardSet,
        _ab: AB,
    ) {
    }
}

#[derive(Debug, Default)]
pub struct UnsyncStats {
    depth: usize,
    start: Option<Instant>,
    elapsed: Option<Duration>,
    guess_iter: u64,
    max_tricks_cutoff: u64,
    nodes: [u64; 13],
    quick_tricks_cutoff: u64,
    search_cutoff: u64,
    search: u64,
    tt_insert: u64,
    tt_hit: u64,
    tt_lower_cutoff: u64,
    tt_upper_cutoff: u64,
}

impl UnsyncStats {
    #[allow(clippy::cast_possible_truncation)]
    fn print(&self, init: &InitialPosition) {
        let total_nodes: u64 = self.nodes.iter().sum();
        let duration = u64::try_from(self.elapsed.unwrap().as_millis()).unwrap_or(u64::MAX);
        println!("Search Statistics:");
        println!(
            "    contract:           {}{}",
            init.declarer.symbol(),
            init.trumps.map_or("N", |s| s.symbol())
        );
        println!("    duration: {duration:12} ms");
        println!("    searches: {:12}", self.search);
        println!("    guesses:  {:12}", self.guess_iter);
        println!(
            "    nodes:    {:12} {}/ms |{}|",
            total_nodes,
            total_nodes / duration,
            render_sparkline(&self.nodes)
        );
        println!(
            "    cuts:     {:12} {}%",
            self.search_cutoff,
            self.search_cutoff * 100 / total_nodes
        );
        println!(
            "    tt ins:   {:12} {}%",
            self.tt_insert,
            self.tt_insert * 100 / total_nodes
        );
        println!(
            "    tt hits:  {:12} {}%",
            self.tt_hit,
            self.tt_hit * 100 / total_nodes
        );
        println!(
            "    tt lower: {:12} {}%",
            self.tt_lower_cutoff,
            self.tt_lower_cutoff * 100 / total_nodes
        );
        println!(
            "    tt upper: {:12} {}%",
            self.tt_upper_cutoff,
            self.tt_upper_cutoff * 100 / total_nodes
        );
        println!(
            "    max cut:  {:12} {}%",
            self.max_tricks_cutoff,
            self.max_tricks_cutoff * 100 / total_nodes
        );
        println!(
            "    quick tr: {:12} {}%",
            self.quick_tricks_cutoff,
            self.quick_tricks_cutoff * 100 / total_nodes
        );
    }
}

impl Observer for UnsyncStats {
    fn node_enter<PoC: PlayOfCards>(&mut self, _target: u8, _state: &PlayState<PoC>) {
        self.nodes[self.depth / 4] += 1;
        self.depth += 1;
    }

    fn node_exit<PoC: PlayOfCards>(
        &mut self,
        _target: u8,
        _state: &PlayState<PoC>,
        _result: &SearchResult,
    ) {
        self.depth -= 1;
    }

    fn tt_lower_cutoff(&mut self) {
        self.tt_lower_cutoff += 1;
    }

    fn tt_upper_cutoff(&mut self) {
        self.tt_upper_cutoff += 1;
    }

    fn guess_iter(&mut self, _target: u8) {
        self.guess_iter += 1;
    }
    fn guess_iter_done(&mut self, _target: u8) {}

    fn max_tricks_cutoff(&mut self) {
        self.max_tricks_cutoff += 1;
    }

    fn quick_tricks_cutoff(&mut self) {
        self.quick_tricks_cutoff += 1;
    }

    fn search_cutoff(&mut self) {
        self.search_cutoff += 1;
    }

    fn search_finished(&mut self, init: &InitialPosition) {
        self.elapsed = Some(self.start.unwrap().elapsed());
        self.print(init);
    }

    fn search_started(&mut self) {
        self.search += 1;
        self.start = Some(Instant::now());
    }

    fn tt_hit<PoC: PlayOfCards>(&mut self, _state: &PlayState<PoC>, _rel_mask: CardSet, _ab: AB) {
        self.tt_hit += 1;
    }

    fn child_enter(&mut self, _card: Card) {}
    fn child_exit(&mut self, _card: Card) {}

    fn update_table<PoC: PlayOfCards>(
        &mut self,
        _state: &PlayState<PoC>,
        _rel_mask: CardSet,
        _ab: AB,
    ) {
        self.tt_insert += 1;
    }
}

#[derive(Default)]
pub struct Logger {
    depth: usize,
}

impl Logger {
    fn indent(&self) {
        for _i in 0..self.depth {
            print!("  ");
        }
    }
}

impl Observer for Logger {
    fn guess_iter(&mut self, _target: u8) {}
    fn guess_iter_done(&mut self, _target: u8) {}

    fn max_tricks_cutoff(&mut self) {}

    fn node_enter<PoC: PlayOfCards>(&mut self, target: u8, state: &PlayState<PoC>) {
        self.indent();
        if state.play.is_empty() {
            print!("* ");
        } else {
            print!("> ");
        }
        println!("{target} {:?} {:?}", state.next, state.deal);
        self.depth += 1;
    }

    fn node_exit<PoC: PlayOfCards>(
        &mut self,
        target: u8,
        state: &PlayState<PoC>,
        result: &SearchResult,
    ) {
        self.depth -= 1;
        self.indent();
        println!(
            "< {target} {:?} {} {} {:?}",
            state.next,
            state.deal.format_bpn_masked(result.rel_mask),
            result.possible,
            result.ab
        );
    }

    fn child_enter(&mut self, card: Card) {
        self.indent();
        println!("{card:?}");
    }

    fn child_exit(&mut self, _card: Card) {}

    fn quick_tricks_cutoff(&mut self) {}

    fn search_cutoff(&mut self) {}

    fn search_finished(&mut self, _init: &InitialPosition) {}

    fn search_started(&mut self) {}

    fn tt_hit<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB) {
        self.indent();
        println!(
            "T {:?} {} {:?}",
            state.next,
            state.deal.format_bpn_masked(rel_mask),
            ab
        );
    }

    fn tt_lower_cutoff(&mut self) {}

    fn tt_upper_cutoff(&mut self) {}

    fn update_table<PoC: PlayOfCards>(
        &mut self,
        state: &PlayState<PoC>,
        rel_mask: CardSet,
        ab: AB,
    ) {
        self.indent();
        println!(
            "! {:?} {} {:?}",
            state.next,
            state.deal.format_bpn_masked(rel_mask),
            ab
        );
    }
}

pub(crate) struct Dot {
    path: Vec<String>,
    writer: BufWriter<File>,
    search: bool,
}
impl Dot {
    pub(crate) fn new(out: &Path) -> Self {
        Self {
            path: vec![],
            writer: BufWriter::new(File::create(out).unwrap()),
            search: false,
        }
    }
}

impl Observer for Dot {
    fn node_enter<PoC: PlayOfCards>(&mut self, _target: u8, state: &PlayState<PoC>) {
        if self.search {
            let id = self.path.join(" ");
            let parent_id = &self.path[..&self.path.len() - 1].join(" ");
            writeln!(
                self.writer,
                "\"{}\" [label=\"{} {}\"]",
                &id,
                state.next.symbol(),
                &state.deal.format_as_bpn()
            )
            .unwrap();
            writeln!(self.writer, "\"{}\" -> \"{}\" [label=\"{}\"]", &parent_id, &id, self.path.last().unwrap()).unwrap();
            if state.play.is_empty() {
                writeln!(
                    self.writer,
                    "\"{}\" [shape=box]",
                    &id,
                )
                .unwrap();    
            }
        }
    }

    fn node_exit<PoC: PlayOfCards>(
        &mut self,
        _target: u8,
        _state: &PlayState<PoC>,
        _result: &SearchResult,
    ) {
    }

    fn child_enter(&mut self, card: Card) {
        self.path.push(format!("{}", card.symbol()));
    }

    fn child_exit(&mut self, card: Card) {
        self.path.pop();
    }

    fn update_table<PoC: PlayOfCards>(
        &mut self,
        state: &PlayState<PoC>,
        rel_mask: CardSet,
        ab: AB,
    ) {
    }

    fn guess_iter(&mut self, target: u8) {
        self.path.push(format!("{target}"));
    }

    fn guess_iter_done(&mut self, _target: u8) {
        self.path.pop();
    }

    fn max_tricks_cutoff(&mut self) {
        if self.search {
            let id = self.path.join(" ");
            writeln!(
                self.writer,
                "\"{}\" [style=filled fillcolor=\"red\"]",
                &id,
            )
            .unwrap();
        }
    }

    fn quick_tricks_cutoff(&mut self) {
        if self.search {
            let id = self.path.join(" ");
            writeln!(
                self.writer,
                "\"{}\" [style=filled fillcolor=\"lightgreen\"]",
                &id,
            )
            .unwrap();
        }
    }

    fn search_cutoff(&mut self) {}

    fn search_finished(&mut self, init: &InitialPosition) {
        writeln!(self.writer, "}}").unwrap();
        self.search = false;
    }

    fn search_started(&mut self) {
        writeln!(self.writer, "strict digraph {{").unwrap();
        self.search = true;
    }

    fn tt_hit<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB) {}

    fn tt_lower_cutoff(&mut self) {
        if self.search {
            let id = self.path.join(" ");
            writeln!(
                self.writer,
                "\"{}\" [style=filled fillcolor=\"lightblue\"]",
                &id,
            )
            .unwrap();
        }
    }

    fn tt_upper_cutoff(&mut self) {
        if self.search {
            let id = self.path.join(" ");
            writeln!(
                self.writer,
                "\"{}\" [style=filled fillcolor=\"lightblue\"]",
                &id,
            )
            .unwrap();
        }
    }
}
