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
                    { name = "one"
                    , type_ = Just "varchar"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Default
                                ( Default'Expression
                                    ( Expression'LiteralValue
                                        ( Number "10" )
                                    )
                                )
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "two"
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