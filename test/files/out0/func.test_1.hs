Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "tbl1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "t1"
                    , type_ = Just "text"
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )