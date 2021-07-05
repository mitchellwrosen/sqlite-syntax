Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t2"
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