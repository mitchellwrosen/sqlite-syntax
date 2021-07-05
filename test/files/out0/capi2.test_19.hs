Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "tab1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "col1"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "col2"
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