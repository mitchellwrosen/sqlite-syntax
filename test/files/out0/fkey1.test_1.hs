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
                    , type_ = Just "INTEGER"
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
                                , value = ColumnConstraint'ForeignKey
                                    ( ForeignKeyClause
                                        { table = "t1"
                                        , columns = Nothing
                                        , onDelete = Cascade
                                        , onUpdate = NoAction
                                        , deferred = False
                                        }
                                    )
                                }
                            , Named
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
                        }
                    , ColumnDefinition
                        { name = "c"
                        , type_ = Just "TEXT"
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'ForeignKey
                            ( "b" :| [ "c" ] )
                            ( ForeignKeyClause
                                { table = "t2"
                                , columns = Just
                                    ( "x" :| [ "y" ] )
                                , onDelete = NoAction
                                , onUpdate = Cascade
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