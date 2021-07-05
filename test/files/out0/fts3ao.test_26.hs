Statement'AlterTable
    ( AlterTableStatement
        { table = Namespaced
            { namespace = Nothing
            , value = "t4"
            }
        , alteration = TableAlteration'Rename "t5"
        }
    )