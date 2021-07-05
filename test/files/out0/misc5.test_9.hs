Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "logs"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "msg"
                    , type_ = Just "TEXT"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "timestamp"
                        , type_ = Just "INTEGER"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "dbtime"
                        , type_ = Just "TEXT"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )