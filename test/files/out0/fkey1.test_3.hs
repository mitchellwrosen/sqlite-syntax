Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t3"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Just "INTEGER"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'ForeignKey
                                ( ForeignKeyClause
                                    { table = "t2"
                                    , columns = Nothing
                                    , onDelete = NoAction
                                    , onUpdate = NoAction
                                    , deferred = False
                                    }
                                )
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Just "INTEGER"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'ForeignKey
                                    ( ForeignKeyClause
                                        { table = "t1"
                                        , columns = Nothing
                                        , onDelete = NoAction
                                        , onUpdate = NoAction
                                        , deferred = False
                                        }
                                    )
                                }
                            ]
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'ForeignKey
                            ( "a" :| [ "b" ] )
                            ( ForeignKeyClause
                                { table = "t2"
                                , columns = Just
                                    ( "x" :| [ "y" ] )
                                , onDelete = NoAction
                                , onUpdate = NoAction
                                , deferred = False
                                }
                            )
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )