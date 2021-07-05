Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "test1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "cnt"
                    , type_ = Just "int"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "power"
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