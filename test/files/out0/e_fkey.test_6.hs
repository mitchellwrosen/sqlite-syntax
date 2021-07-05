Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "c"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "j"
                    , type_ = Nothing
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'ForeignKey
                                ( ForeignKeyClause
                                    { table = "p"
                                    , columns = Nothing
                                    , onDelete = NoAction
                                    , onUpdate = NoAction
                                    , deferred = False
                                    }
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