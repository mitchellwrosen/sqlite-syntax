Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = True
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "Items"
                                            }
                                        )
                                    , value = "Item"
                                    }
                                )
                            , alias = Just "trove"
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "UP"
                                                }
                                            )
                                        , value = "pattern"
                                        }
                                    )
                                , alias = Just "pattern"
                                }
                            )
                        ]
                    , from = Just
                        ( Table'InnerJoin
                            ( Table'InnerJoin
                                ( Table'InnerJoin
                                    ( Table'Subquery
                                        ( Aliased
                                            { value = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "Permissions"
                                                                                    }
                                                                                )
                                                                            , value = "labelId"
                                                                            }
                                                                        )
                                                                    , alias = Just "labelId"
                                                                    }
                                                                ) :|
                                                                [ ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "PerItems"
                                                                                        }
                                                                                    )
                                                                                , value = "item"
                                                                                }
                                                                            )
                                                                        , alias = Just "pattern"
                                                                        }
                                                                    )
                                                                ]
                                                            , from = Just
                                                                ( Table'LeftOuterJoin
                                                                    ( Table'InnerJoin
                                                                        ( Table'InnerJoin
                                                                            ( Table
                                                                                ( QualifiedTableName
                                                                                    { name = Aliased
                                                                                        { value = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "Users"
                                                                                            }
                                                                                        , alias = Nothing
                                                                                        }
                                                                                    , indexedBy = Nothing
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Table
                                                                                ( QualifiedTableName
                                                                                    { name = Aliased
                                                                                        { value = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "UserGroupMembers"
                                                                                            }
                                                                                        , alias = Nothing
                                                                                        }
                                                                                    , indexedBy = Nothing
                                                                                    }
                                                                                )
                                                                            ) Nothing
                                                                        )
                                                                        ( Table
                                                                            ( QualifiedTableName
                                                                                { name = Aliased
                                                                                    { value = Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "Permissions"
                                                                                        }
                                                                                    , alias = Nothing
                                                                                    }
                                                                                , indexedBy = Nothing
                                                                                }
                                                                            )
                                                                        ) Nothing
                                                                    )
                                                                    ( Table
                                                                        ( QualifiedTableName
                                                                            { name = Aliased
                                                                                { value = Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "Items"
                                                                                    }
                                                                                , alias = Just "PerItems"
                                                                                }
                                                                            , indexedBy = Nothing
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Just
                                                                        ( On
                                                                            ( Expression'Equals
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Just
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "Permissions"
                                                                                                }
                                                                                            )
                                                                                        , value = "itemId"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Just
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "PerItems"
                                                                                                }
                                                                                            )
                                                                                        , value = "itemId"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            , where_ = Just
                                                                ( Expression'And
                                                                    ( Expression'And
                                                                        ( Expression'Equals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "Users"
                                                                                            }
                                                                                        )
                                                                                    , value = "user"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'LiteralValue
                                                                                ( String "limited" )
                                                                            )
                                                                        )
                                                                        ( Expression'Equals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "Users"
                                                                                            }
                                                                                        )
                                                                                    , value = "userId"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "UserGroupMembers"
                                                                                            }
                                                                                        )
                                                                                    , value = "userId"
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Equals
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "UserGroupMembers"
                                                                                        }
                                                                                    )
                                                                                , value = "userGroupId"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "Permissions"
                                                                                        }
                                                                                    )
                                                                                , value = "userGroupId"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            , groupBy = Nothing
                                                            , window = Nothing
                                                            }
                                                        )
                                                    )
                                                , orderBy = Nothing
                                                , limit = Nothing
                                                }
                                            , alias = Just "UP"
                                            }
                                        )
                                    )
                                    ( Table
                                        ( QualifiedTableName
                                            { name = Aliased
                                                { value = Namespaced
                                                    { namespace = Nothing
                                                    , value = "LabelMap"
                                                    }
                                                , alias = Nothing
                                                }
                                            , indexedBy = Nothing
                                            }
                                        )
                                    )
                                    ( Just
                                        ( On
                                            ( Expression'Or
                                                ( Expression'Equals
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "UP"
                                                                    }
                                                                )
                                                            , value = "labelId"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "0" )
                                                    )
                                                )
                                                ( Expression'Equals
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "UP"
                                                                    }
                                                                )
                                                            , value = "labelId"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "LabelMap"
                                                                    }
                                                                )
                                                            , value = "labelId"
                                                            }
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "Labels"
                                                }
                                            , alias = Nothing
                                            }
                                        , indexedBy = Nothing
                                        }
                                    )
                                ) Nothing
                            )
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "Items"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'And
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "Labels"
                                                    }
                                                )
                                            , value = "label"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "localhost@rpl:branch" )
                                    )
                                )
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "Labels"
                                                    }
                                                )
                                            , value = "labelId"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "LabelMap"
                                                    }
                                                )
                                            , value = "labelId"
                                            }
                                        )
                                    )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "LabelMap"
                                                }
                                            )
                                        , value = "itemId"
                                        }
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "Items"
                                                }
                                            )
                                        , value = "itemId"
                                        }
                                    )
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "trove"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "pattern"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                ]
            )
        , limit = Nothing
        }
    )