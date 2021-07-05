Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "Folders"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "folderid"
                    , type_ = Just "INTEGER"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'PrimaryKey Ascending Abort False
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "parentid"
                        , type_ = Just "INTEGER"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "rootid"
                        , type_ = Just "INTEGER"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "path"
                        , type_ = Just "VARCHAR"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'Default
                                    ( Default'Expression
                                        ( Expression'LiteralValue
                                            ( Number "255" )
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