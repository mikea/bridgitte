use std::{fs::read_to_string, path::PathBuf};

use bridgitte::{
    analyze::{analyze_all, analyze_player, analyze_strain, ByStrain, Strain},
    minmax::minmax,
    search::{Config, InitialPosition, TracingConfig, TracingType},
    trans_table::Pattern,
    ByPlayer, Deal, Player, Suit,
};
use clap::{Args, Parser, Subcommand};
use rayon::prelude::*;
use regex::Regex;

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    AnalyzeDeal {
        deal: String,
        #[arg(short, long)]
        declarer: Option<Player>,
        #[arg(short, long)]
        next: Option<Player>,
        #[arg(short, long)]
        strain: Option<Strain>,
        #[arg(long)]
        trace: Option<TracingType>,
    },
    TestPattern(TestPatternArgs),
    Test(TestArgs),
}

#[derive(Args, Debug)]
struct TestPatternArgs {
    pattern: Pattern,
    #[arg(short, long)]
    strain: Option<Suit>,
    #[arg(short, long, default_value_t = 1000usize)]
    iter: usize,
}

#[derive(Args, Debug)]
struct TestArgs {
    input: PathBuf,
}

pub fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::AnalyzeDeal {
            deal,
            declarer,
            strain,
            trace,
            next,
        } => {
            let deal = Deal::try_from_pbn(&deal).unwrap();
            println!("{}", deal.format_as_table());
            println!();

            let config = Config {
                tracing: trace.map(|t| TracingConfig {
                    typ: t,
                    output: PathBuf::from("trace"),
                }),
            };

            if let Some(declarer) = declarer {
                if let Some(strain) = strain {
                    let mut s = InitialPosition {
                        deal,
                        trumps: strain.trumps,
                        declarer,
                        next,
                    }
                    .search(&config);
                    let analysis = s.run_unbounded();
                    println!();
                    println!("   {strain:?}",);
                    println!("{declarer:?} {analysis:2}");
                    println!();
                    let line = s.optimal_line(analysis);
                    println!("     W N E S");
                    for (i, (player, cards)) in line.iter().enumerate() {
                        let cards = cards.map(|c| c.symbol().to_string()).join(" ");
                        println!("{:2} {player:?} {}", i + 1, cards);
                    }
                } else {
                    let analysis = analyze_player(&deal, declarer, &config);
                    print_by_strain(analysis, declarer);
                }
            } else if let Some(strain) = strain {
                let analysis = analyze_strain(&deal, &strain, &config);
                print_by_player(&analysis);
            } else {
                let analysis = analyze_all(&deal, &config);
                print_analysis(&analysis);
            }
        }
        Command::TestPattern(args) => {
            test_pattern(&args);
        }
        Command::Test(args) => {
            run_test(&args);
        }
    }
}

fn run_test(args: &TestArgs) {
    let test_file = parse(&read_to_string(&args.input).unwrap());
    test_file.run_tests();
}

fn test_pattern(args: &TestPatternArgs) {
    let TestPatternArgs {
        pattern,
        strain,
        iter,
    } = args;

    let mut found = false;
    for _ in 0..*iter {
        let deal = pattern.gen_random_deal();
        let x = minmax(&deal, *strain, pattern.next());
        if !pattern.ab().contains(x) {
            found = true;
            println!("counterexample: {deal:?} = {x}");
        }
    }

    if found {
        panic!("the pattern is inconsistent");
    } else {
        println!("no counter-examples found in {iter} iterations");
    }
}

fn print_analysis(analysis: &bridgitte::ByPlayer<ByStrain<u8>>) {
    println!(
        "   {}  {}  {}  {}  N",
        Suit::C.symbol(),
        Suit::D.symbol(),
        Suit::H.symbol(),
        Suit::S.symbol()
    );
    println!(
        "W {:2} {:2} {:2} {:2} {:2}",
        analysis.w.c, analysis.w.d, analysis.w.h, analysis.w.s, analysis.w.n
    );
    println!(
        "N {:2} {:2} {:2} {:2} {:2}",
        analysis.n.c, analysis.n.d, analysis.n.h, analysis.n.s, analysis.n.n
    );
    println!(
        "E {:2} {:2} {:2} {:2} {:2}",
        analysis.e.c, analysis.e.d, analysis.e.h, analysis.e.s, analysis.e.n
    );
    println!(
        "S {:2} {:2} {:2} {:2} {:2}",
        analysis.s.c, analysis.s.d, analysis.s.h, analysis.s.s, analysis.s.n
    );
}

fn print_by_player(analysis: &bridgitte::ByPlayer<u8>) {
    println!("   W   N   E   S");
    println!(
        "{:2} {:2} {:2} {:2}",
        analysis.w, analysis.n, analysis.e, analysis.s
    );
}

fn print_by_strain(analysis: ByStrain<u8>, player: Player) {
    println!(
        "   {}  {}  {}  {}  N",
        Suit::C.symbol(),
        Suit::D.symbol(),
        Suit::H.symbol(),
        Suit::S.symbol()
    );
    println!(
        "{player:?} {:2} {:2} {:2} {:2} {:2}",
        analysis.c, analysis.d, analysis.h, analysis.s, analysis.n
    );
}

#[derive(Debug)]
struct TestFile {
    tests: Vec<Test>,
}
impl TestFile {
    fn run_tests(&self) {
        self.tests.par_iter().for_each(|test| {
            test.run();
        });
        println!("{} tests pass", self.tests.len());
    }
}

#[derive(Debug)]
struct Test {
    deal: Deal,
    table: ByPlayer<ByStrain<u8>>,
}
impl Test {
    fn run(&self) {
        let result = analyze_all(&self.deal, &Config::default());
        assert_eq!(self.table, result);
    }
}

fn parse(content: &str) -> TestFile {
    let pbn_re = Regex::new(r#"PBN\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+"(.+)""#).unwrap();

    let mut tests = vec![];

    let mut deal = None;
    for line in content.lines() {
        if line.starts_with("NUMBER ")
            || line.starts_with("FUT ")
            || line.starts_with("PAR ")
            || line.starts_with("PAR2 ")
            || line.starts_with("PLAY ")
            || line.starts_with("TRACE ")
        {
            continue;
        }

        if let Some(caps) = pbn_re.captures(line) {
            let (_, [_dealer, _vul, _trump, _first, d]) = caps.extract();
            deal = Some(Deal::try_from_pbn(d).expect("bad deal"));
            continue;
        }

        if let Some(table) = line.strip_prefix("TABLE ") {
            let table: Vec<_> = table
                .trim()
                .split(' ')
                .map(|i| i.parse::<u8>().unwrap())
                .collect();
            assert_eq!(table.len(), 20);
            let table = ByPlayer {
                n: ByStrain {
                    c: table[12],
                    d: table[8],
                    h: table[4],
                    s: table[0],
                    n: table[16],
                },
                e: ByStrain {
                    c: table[13],
                    d: table[9],
                    h: table[5],
                    s: table[1],
                    n: table[17],
                },
                s: ByStrain {
                    c: table[14],
                    d: table[10],
                    h: table[6],
                    s: table[2],
                    n: table[18],
                },
                w: ByStrain {
                    c: table[15],
                    d: table[11],
                    h: table[7],
                    s: table[3],
                    n: table[19],
                },
            };
            tests.push(Test {
                deal: deal.unwrap(),
                table,
            });
            deal = None;
            continue;
        }

        panic!("unexpected line: {line}");
    }
    TestFile { tests }
}
