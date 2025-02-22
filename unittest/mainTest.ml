open OUnit2

let all_tests = ("" >:::
  [ IntDomainTest.test ();
    FloatDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    LvalTest.test ();
    CompilationDatabaseTest.tests;
    LibraryDslTest.tests;
    (* etc *)
    "domaintest" >::: QCheck_ounit.to_ounit2_test_list Maindomaintest.all_testsuite;
    IntOpsTest.tests;
  ])

let () = run_test_tt_main all_tests
