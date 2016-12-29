package excel.engine.model.message;

/**
 * excel data valid message
 * <p>
 * Created by hanwen on 15-12-15.
 */
public class DataValidateMessage implements ValidateMessage {

  private String errorMessage;

  private boolean writeComment;

  private Integer sheetIndex;

  private Integer rowIndex;

  private Integer columnIndex;

  @Override
  public String getMessage() {
    return errorMessage;
  }

}
