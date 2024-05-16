val with_test_dir : (OUnit2.test_ctxt -> string -> 'a) -> OUnit2.test_ctxt -> 'a
(** [with_test_dir f ctx] allows for tests to be run with a specific
    context--specifically, designed to allow tests read/writing files to
    read/write to/from specified directories. *)
