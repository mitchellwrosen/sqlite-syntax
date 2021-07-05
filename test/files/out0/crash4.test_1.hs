Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "a"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "id"
                    , type_ = Just "INTEGER"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "name"
                        , type_ = Just "CHAR"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'Default
                                    ( Default'Expression
                                        ( Expression'LiteralValue
                                            ( Number "50" )
                                        )
                                    )
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