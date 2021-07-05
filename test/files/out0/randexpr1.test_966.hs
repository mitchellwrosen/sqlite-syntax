Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Multiply
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "e"
                                        }
                                    )
                                )
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Just
                                            ( Expression'Case
                                                ( CaseExpression
                                                    { base = Nothing
                                                    , cases =
                                                        ( Expression'Not
                                                            ( Expression'Or
                                                                ( Expression'Or
                                                                    ( Expression'Or
                                                                        ( Expression'Or
                                                                            ( Expression'GreaterThanOrEquals
                                                                                ( Expression'Subquery
                                                                                    ( SelectStatement
                                                                                        { commonTableExpressions = Nothing
                                                                                        , select = CompoundSelect
                                                                                            ( SelectCore'Select
                                                                                                ( Select
                                                                                                    { distinct = False
                                                                                                    , columns = ResultColumn'Expression
                                                                                                        ( Aliased
                                                                                                            { value = Expression'FunctionCall
                                                                                                                ( FunctionCallExpression
                                                                                                                    { call = FunctionCall
                                                                                                                        { name = Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "max"
                                                                                                                            }
                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                            [ Expression'LiteralValue
                                                                                                                                ( Number "17" )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    , filter = Nothing
                                                                                                                    , over = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            , alias = Nothing
                                                                                                            }
                                                                                                        ) :| []
                                                                                                    , from = Just
                                                                                                        ( Table
                                                                                                            ( QualifiedTableName
                                                                                                                { name = Aliased
                                                                                                                    { value = Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    , alias = Nothing
                                                                                                                    }
                                                                                                                , indexedBy = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , where_ = Nothing
                                                                                                    , groupBy = Nothing
                                                                                                    , window = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        , orderBy = Nothing
                                                                                        , limit = Nothing
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'Case
                                                                                    ( CaseExpression
                                                                                        { base = Nothing
                                                                                        , cases =
                                                                                            ( Expression'Not
                                                                                                ( Expression'Equals
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "d"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'Plus
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "c"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Just
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , value = "c"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                            , Expression'LiteralValue
                                                                                                ( Number "11" )
                                                                                            ) :| []
                                                                                        , else_ = Expression'LiteralValue
                                                                                            ( Number "11" )
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Or
                                                                                ( Expression'InSubquery
                                                                                    ( InSubqueryExpression
                                                                                        { expression = Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        , subquery = SelectStatement
                                                                                            { commonTableExpressions = Nothing
                                                                                            , select = Union
                                                                                                ( CompoundSelect
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
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "c"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , alias = Nothing
                                                                                                                    }
                                                                                                                ) :| []
                                                                                                            , from = Just
                                                                                                                ( Table
                                                                                                                    ( QualifiedTableName
                                                                                                                        { name = Aliased
                                                                                                                            { value = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "t1"
                                                                                                                                }
                                                                                                                            , alias = Nothing
                                                                                                                            }
                                                                                                                        , indexedBy = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            , where_ = Nothing
                                                                                                            , groupBy = Nothing
                                                                                                            , window = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                                ( SelectCore'Select
                                                                                                    ( Select
                                                                                                        { distinct = False
                                                                                                        , columns = ResultColumn'Expression
                                                                                                            ( Aliased
                                                                                                                { value = Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "c"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , alias = Nothing
                                                                                                                }
                                                                                                            ) :| []
                                                                                                        , from = Just
                                                                                                            ( Table
                                                                                                                ( QualifiedTableName
                                                                                                                    { name = Aliased
                                                                                                                        { value = Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "t1"
                                                                                                                            }
                                                                                                                        , alias = Nothing
                                                                                                                        }
                                                                                                                    , indexedBy = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , where_ = Nothing
                                                                                                        , groupBy = Nothing
                                                                                                        , window = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            , orderBy = Nothing
                                                                                            , limit = Nothing
                                                                                            }
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'LessThanOrEquals
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "11" )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "a"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'LessThanOrEquals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "d"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'LiteralValue
                                                                                ( Number "11" )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'InValues
                                                                        ( InValuesExpression
                                                                            { expression = Expression'LiteralValue
                                                                                ( Number "19" )
                                                                            , values =
                                                                                [ Expression'Negate
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "13" )
                                                                                    )
                                                                                , Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
                                                                                        }
                                                                                    )
                                                                                , Expression'LiteralValue
                                                                                    ( Number "19" )
                                                                                ]
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                                ( Expression'Not
                                                                    ( Expression'Between
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "a"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "13" )
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "t1"
                                                                                        }
                                                                                    )
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        , Expression'Multiply
                                                            ( Expression'Case
                                                                ( CaseExpression
                                                                    { base = Nothing
                                                                    , cases =
                                                                        ( Expression'LessThanOrEquals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "f"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "a"
                                                                                    }
                                                                                )
                                                                            )
                                                                        , Expression'Plus
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "e"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Multiply
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "13" )
                                                                                )
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "e"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        ) :|
                                                                        [
                                                                            ( Expression'Not
                                                                                ( Expression'Between
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "b"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "f"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "a"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                            , Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "c"
                                                                                    }
                                                                                )
                                                                            )
                                                                        ]
                                                                    , else_ = Expression'LiteralValue
                                                                        ( Number "11" )
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "11" )
                                                            )
                                                        ) :| []
                                                    , else_ = Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "a"
                                                            }
                                                        )
                                                    }
                                                )
                                            )
                                        , cases =
                                            ( Expression'LiteralValue
                                                ( Number "19" )
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "a"
                                                    }
                                                )
                                            ) :| []
                                        , else_ = Expression'LiteralValue
                                            ( Number "17" )
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'NotEquals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "b"
                                    }
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "17" )
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
    )