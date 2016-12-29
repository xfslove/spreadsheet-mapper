package java.excel.engine.model.message;

import java.util.HashSet;
import java.util.Set;

/**
 * excel data valid message
 * <p>
 * Created by hanwen on 15-12-15.
 */
public class DataValidateMessage implements ValidateMessage {

  private String message;

  private Set<String> messageOnFields = new HashSet<>();

  public DataValidateMessage(String message, Set<String> messageOnFields) {
    this.message = message;
    this.messageOnFields = messageOnFields;
  }

  @Override
  public String getMessage() {
    return message;
  }

  @Override
  public Set<String> getMessageOnFields() {
    return messageOnFields;
  }
}
