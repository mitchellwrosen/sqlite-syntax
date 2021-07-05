Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "t"
                    , type_ = Just "text"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Just "blob"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )