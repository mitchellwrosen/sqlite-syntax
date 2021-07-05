Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "three"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "g"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "h"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "i"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'ForeignKey
                            ( "h" :| [ "i" ] )
                            ( ForeignKeyClause
                                { table = "one"
                                , columns = Just
                                    ( "b" :| [ "c" ] )
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