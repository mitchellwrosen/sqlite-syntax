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
                    { name = "a"
                    , type_ = Just "integer"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'PrimaryKey Ascending Abort False
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Just "INTEGER"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'NotNull Abort
                                }
                            ]
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )