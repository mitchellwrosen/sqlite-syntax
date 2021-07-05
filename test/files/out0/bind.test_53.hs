Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t4"
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
                    , ColumnDefinition
                        { name = "d"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "e"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "f"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "g"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "h"
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