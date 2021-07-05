Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "d1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "n"
                    , type_ = Just "int"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "log"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )