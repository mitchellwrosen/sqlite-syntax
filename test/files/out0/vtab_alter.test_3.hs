Statement'AlterTable
    ( AlterTableStatement
        { table = Namespaced
            { namespace = Nothing
            , value = "t1echo"
            }
        , alteration = TableAlteration'Rename "new"
        }
    )