Statement'AlterTable
    ( AlterTableStatement
        { table = Namespaced
            { namespace = Nothing
            , value = "t2"
            }
        , alteration = TableAlteration'Rename "t5"
        }
    )