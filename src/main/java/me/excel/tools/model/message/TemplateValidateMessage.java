package me.excel.tools.model.message;

/**
 * excel template validate message
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public class TemplateValidateMessage implements ValidateMessage {

  public static final TemplateValidateMessage SUCCESS = new TemplateValidateMessage(ValidateResult.SUCCESS, null);

  private ValidateResult result;

  private String message;

  public TemplateValidateMessage(ValidateResult result, String message) {
    this.result = result;
    this.message = message;
  }

  public ValidateResult getResult() {
    return result;
  }

  public String getMessage() {
    return message;
  }
}
