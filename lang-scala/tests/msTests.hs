-- 1) paper example: pass
-- object a {
--   object b {
--     def f(): Unit = {};
--   };
-- };

-- object c {
--   import a._;
--   import b.f;
--   def g(): Unit = { f() };
-- };

-- 6) qualified reference to value: pass
-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object O {
--   val x : Int = A.B.x;
-- };

-- 7) deep-specific-import: pass
-- object O1 {
--   type T = Boolean;
--   object O2 {
--     type T = Int;
--     object O3 {
--       object O4 {
--         object O5 {
--           val x : T = 42;
--         };
--       };
--     };
--   };
-- };

-- object N {
--   import O1.O2.O3.O4.O5.x;
--   val y : Int = x;
-- };