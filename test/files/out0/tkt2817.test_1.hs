Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "tbl"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "c"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )