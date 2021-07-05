Statement'AlterTable
    ( AlterTableStatement
        { table = Namespaced
            { namespace = Nothing
            , value = "t1"
            }
        , alteration = TableAlteration'Rename "x"
        }
    )