Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t6b"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Nothing
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Check
                                ( Expression'NotEquals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "x"
                                            }
                                        )
                                    )
                                    ( Expression'Collate
                                        ( CollateExpression
                                            { expression = Expression'LiteralValue
                                                ( String "abc" )
                                            , collation = "nocase"
                                            }
                                        )
                                    )
                                )
                            }
                        ]
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )