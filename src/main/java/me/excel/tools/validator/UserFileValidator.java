package me.excel.tools.validator;

/**
 * file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileValidator {

  boolean validate();

  void writeFailureMessageComments();
}
