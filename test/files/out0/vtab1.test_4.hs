Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t2152b"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "y"
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