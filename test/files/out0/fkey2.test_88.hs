Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "cc"
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
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'ForeignKey
                            ( "a" :| [ "b" ] )
                            ( ForeignKeyClause
                                { table = "pp"
                                , columns = Nothing
                                , onDelete = NoAction
                                , onUpdate = NoAction
                                , deferred = True
                                }
                            )
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )