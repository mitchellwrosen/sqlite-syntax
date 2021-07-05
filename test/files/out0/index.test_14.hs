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
                    { name = "f1"
                    , type_ = Just "int"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "f2"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "f3"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "f4"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "f5"
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