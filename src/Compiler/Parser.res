/* This is the parser of the following grammar:
 *
 * e  ::= λx.e                -- Lambda abstraction
 *      | (x: e) → e          -- Pi type
 *      | e e                 -- Application
 *      | x                   -- Var
 *      | C                   -- Constructor
 *      | U                   -- Type universe
 *      | x : x               -- Annotation
 *
 * p ::= x                    -- Pattern variable
 *     | _                    -- Wildcard
 *     | C p*                  -- Constructor
 *
 * d  ::= x : e (| p+ = e )*  -- Definition
 * 
 * b  ::= (x : e)             -- Binder
 *
 * dc ::= | C b* = e
 *
 * t  ::= type C b* dc*       -- Type family definition
 *
 */

type error = {
    location: int
}

