package me.excel.tools.validator;

import java.io.File;
import java.io.IOException;

/**
 * file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileValidator {

  boolean validate(File file) throws IOException;

  void writeFailureMessageComments(File file) throws IOException;
}
