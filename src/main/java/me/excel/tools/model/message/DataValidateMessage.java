package me.excel.tools.model.message;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * excel data validate message
 * <p>
 * Created by hanwen on 15-12-15.
 */
public class DataValidateMessage implements ValidateMessage {

  public static final DataValidateMessage SUCCESS = new DataValidateMessage(ValidateResult.SUCCESS, null, Collections.emptySet());

  private ValidateResult result;

  private String message;

  private Set<String> messageOnFields = new HashSet<>();

  public DataValidateMessage(ValidateResult result, String message, Set<String> messageOnFields) {
    this.result = result;
    this.message = message;
    this.messageOnFields = messageOnFields;
  }

  @Override
  public ValidateResult getResult() {
    return result;
  }

  @Override
  public String getMessage() {
    return message;
  }

  /**
   * @return message on which fields
   */
  public Set<String> getMessageOnFields() {
    return messageOnFields;
  }
}
