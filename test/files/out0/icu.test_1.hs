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
                    { name = "i1"
                    , type_ = Just "int"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "i2"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "r1"
                        , type_ = Just "real"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "r2"
                        , type_ = Just "real"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "t1"
                        , type_ = Just "text"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "t2"
                        , type_ = Just "text"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )