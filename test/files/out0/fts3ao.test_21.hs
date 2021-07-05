Statement'AlterTable
    ( AlterTableStatement
        { table = Namespaced
            { namespace = Just "aux"
            , value = "t1"
            }
        , alteration = TableAlteration'Rename "t2"
        }
    )