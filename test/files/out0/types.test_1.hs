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
                    { name = "i"
                    , type_ = Just "integer"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "n"
                        , type_ = Just "numeric"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "t"
                        , type_ = Just "text"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "o"
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