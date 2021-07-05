Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "elephant"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "name"
                    , type_ = Just "VARCHAR"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Default
                                ( Default'Expression
                                    ( Expression'LiteralValue
                                        ( Number "32" )
                                    )
                                )
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "color"
                        , type_ = Just "VARCHAR"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'Default
                                    ( Default'Expression
                                        ( Expression'LiteralValue
                                            ( Number "16" )
                                        )
                                    )
                                }
                            ]
                        }
                    , ColumnDefinition
                        { name = "age"
                        , type_ = Just "INTEGER"
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'Unique
                            ( IndexedColumn
                                { column = "name"
                                , collation = Nothing
                                , ordering = Ascending
                                } :|
                                [ IndexedColumn
                                    { column = "color"
                                    , collation = Nothing
                                    , ordering = Ascending
                                    }
                                ]
                            ) Abort
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )