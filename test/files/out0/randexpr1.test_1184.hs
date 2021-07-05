Statement'Select
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
                                            , value = "coalesce"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Subquery
                                                ( SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = CompoundSelect
                                                        ( SelectCore'Select
                                                            ( Select
                                                                { distinct = False
                                                                , columns = ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'Case
                                                                            ( CaseExpression
                                                                                { base = Nothing
                                                                                , cases =
                                                                                    ( Expression'GreaterThan
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "f"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'Minus
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "17" )
                                                                                            )
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
                                                                                        )
                                                                                    , Expression'Subquery
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
                                                                                                                                [ Expression'FunctionCall
                                                                                                                                    ( FunctionCallExpression
                                                                                                                                        { call = FunctionCall
                                                                                                                                            { name = Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "coalesce"
                                                                                                                                                }
                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                [ Expression'Subquery
                                                                                                                                                    ( SelectStatement
                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                        , select = CompoundSelect
                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                ( Select
                                                                                                                                                                    { distinct = False
                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                        ( Aliased
                                                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                                                ( Number "11" )
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
                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                            ( Expression'InValues
                                                                                                                                                                                ( InValuesExpression
                                                                                                                                                                                    { expression = Expression'FunctionCall
                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "coalesce"
                                                                                                                                                                                                    }
                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                    [ Expression'Subquery
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
                                                                                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "d"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    )
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
                                                                                                                                                                                                                        , where_ = Just
                                                                                                                                                                                                                            ( Expression'Between
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
                                                                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                                                                        }
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
                                                                                                                                                                                                        )
                                                                                                                                                                                                    , Expression'Column
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
                                                                                                                                                                                                    ]
                                                                                                                                                                                                }
                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    , values =
                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , Expression'Negate
                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "f"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        , Expression'Column
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
                                                                                                                                                                                        ]
                                                                                                                                                                                    }
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
                                                                                                                                                    )
                                                                                                                                                , Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "d"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                ]
                                                                                                                                            }
                                                                                                                                        , filter = Nothing
                                                                                                                                        , over = Nothing
                                                                                                                                        }
                                                                                                                                    )
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
                                                                                    ) :| []
                                                                                , else_ = Expression'Plus
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "17" )
                                                                                    )
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "19" )
                                                                                    )
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
                                                                , where_ = Just
                                                                    ( Expression'Not
                                                                        ( Expression'Not
                                                                            ( Expression'Exists
                                                                                ( SelectStatement
                                                                                    { commonTableExpressions = Nothing
                                                                                    , select = CompoundSelect
                                                                                        ( SelectCore'Select
                                                                                            ( Select
                                                                                                { distinct = False
                                                                                                , columns = ResultColumn'Expression
                                                                                                    ( Aliased
                                                                                                        { value = Expression'LiteralValue
                                                                                                            ( Number "1" )
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
                                                                                                    ( Expression'Or
                                                                                                        ( Expression'Or
                                                                                                            ( Expression'Or
                                                                                                                ( Expression'And
                                                                                                                    ( Expression'InSubquery
                                                                                                                        ( InSubqueryExpression
                                                                                                                            { expression = Expression'Column
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
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "a"
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
                                                                                                                                                            { namespace = Just
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "t1"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , value = "a"
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
                                                                                                                    ( Expression'Equals
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'And
                                                                                                                    ( Expression'LessThanOrEquals
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        )
                                                                                                                        ( Expression'Negate
                                                                                                                            ( Expression'LiteralValue
                                                                                                                                ( Number "19" )
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'InSubquery
                                                                                                                        ( InSubqueryExpression
                                                                                                                            { expression = Expression'Column
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
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "d"
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
                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                        ( Number "13" )
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
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'GreaterThan
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "19" )
                                                                                                                )
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Not
                                                                                                            ( Expression'InValues
                                                                                                                ( InValuesExpression
                                                                                                                    { expression = Expression'Column
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
                                                                                                                    , values =
                                                                                                                        [ Expression'Column
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
                                                                                                                        , Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        ]
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
                                                )
                                            , Expression'Negate
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
                    , where_ = Just
                        ( Expression'NotEquals
                            ( Expression'Minus
                                ( Expression'Minus
                                    ( Expression'Plus
                                        ( Expression'Minus
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
                                            ( Expression'LiteralValue
                                                ( Number "13" )
                                            )
                                        )
                                        ( Expression'Subquery
                                            ( SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'AggregateDistinctFunctionCall
                                                                        ( AggregateDistinctFunctionCallExpression
                                                                            { call = FunctionCall
                                                                                { name = Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "count"
                                                                                    }
                                                                                , arguments = Identity
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "a"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                }
                                                                            , filter = Nothing
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
                                    )
                                    ( Expression'Multiply
                                        ( Expression'Multiply
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'Case
                                            ( CaseExpression
                                                { base = Nothing
                                                , cases =
                                                    ( Expression'LessThanOrEquals
                                                        ( Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "coalesce"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Subquery
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
                                                                                                                    [ Expression'FunctionCall
                                                                                                                        ( FunctionCallExpression
                                                                                                                            { call = FunctionCall
                                                                                                                                { name = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "coalesce"
                                                                                                                                    }
                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                    [ Expression'Subquery
                                                                                                                                        ( SelectStatement
                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                            , select = CompoundSelect
                                                                                                                                                ( SelectCore'Select
                                                                                                                                                    ( Select
                                                                                                                                                        { distinct = False
                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                            ( Aliased
                                                                                                                                                                { value = Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "a"
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
                                                                                                                                                        , where_ = Just
                                                                                                                                                            ( Expression'NotEquals
                                                                                                                                                                ( Expression'Case
                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                        { base = Just
                                                                                                                                                                            ( Expression'Minus
                                                                                                                                                                                ( Expression'Plus
                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "b"
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
                                                                                                                                                                                ( Expression'Multiply
                                                                                                                                                                                    ( Expression'Multiply
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
                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "d"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                            )
                                                                                                                                                                        , cases =
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
                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "f"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "c"
                                                                                                                                                                        }
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
                                                                                                                                                , value = "b"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                            ( Number "13" )
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            , filter = Nothing
                                                                                                                            , over = Nothing
                                                                                                                            }
                                                                                                                        )
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
                                                                                            , where_ = Just
                                                                                                ( Expression'Between
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
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "17" )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Just
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "t1"
                                                                                                                    }
                                                                                                                )
                                                                                                            , value = "d"
                                                                                                            }
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
                                                                            )
                                                                        , Expression'Column
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
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        )
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "b"
                                                                }
                                                            )
                                                        )
                                                    , Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "f"
                                                            }
                                                        )
                                                    ) :| []
                                                , else_ = Expression'Column
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
                                                }
                                            )
                                        )
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "13" )
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
                                    , value = "f"
                                    }
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
    )