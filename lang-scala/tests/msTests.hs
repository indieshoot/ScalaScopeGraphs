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

-- 2) mutual-defs: pass
-- object A {
--   def f: Int = g;
--   def g: Int = f;
-- };

-- 3) recursive-defs: pass
-- object A {
--   def f: Int = f;
-- };

-- 4) nested-val-defs:
-- object O {
--   val x : Int = 42;
--   object P {
--     val x : Int = 43;
--   };
-- };

-- 5) mutual-import-objects:
-- object A {
--   import B._;
--   val x : Int = y;
-- };

-- object B {
--   import A._;
--   val y : Int = x;
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

-- ds <- query s reImpRes pShortest (matchDecl objName)
        -- We need to match to determine ambiguity.
        -- case ds of
        --   [] -> err $ "Object '" ++ objName ++ "' does not exist."
        --   [ObjDecl _ s'] -> do
        --     ds' <- query s' re pShortest (matchDecl varName) <&> map projTy
        --     case ds' of
        --       [] -> err "No matching declarations found"
        --       [t] -> do
        --         -- copy name in our scope
        --         sink s EI $ Decl varName t
        --       _ -> err "BUG: Multiple declarations found" -- cannot happen for STLC
        --   _ -> err $ "Multiple objects '" ++ objName ++ "' exist."