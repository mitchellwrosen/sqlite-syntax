Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "abc"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Nothing
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'PrimaryKey Ascending Abort False
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "b"
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