package spread.sheet.model.message;

/**
 * Created by hanwen on 2017/1/3.
 */
public class ErrorMessageBean implements ErrorMessage {

  private String messageWriteStrategy;

  private String errorMessage;

  private int sheetIndex;

  private Integer rowIndex;

  private Integer columnIndex;

  public ErrorMessageBean(String messageWriteStrategy, String errorMessage, int sheetIndex) {
    this.messageWriteStrategy = messageWriteStrategy;
    this.errorMessage = errorMessage;
    this.sheetIndex = sheetIndex;
  }

  public ErrorMessageBean(String messageWriteStrategy, String errorMessage, int sheetIndex, Integer rowIndex, Integer columnIndex) {
    if (rowIndex == null ^ columnIndex == null) {
      throw new IllegalArgumentException("row index and column index must both null or both not null");
    }

    this.messageWriteStrategy = messageWriteStrategy;
    this.errorMessage = errorMessage;
    this.sheetIndex = sheetIndex;
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
  }

  @Override
  public String getMessageWriteStrategy() {
    return messageWriteStrategy;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public Integer getRowIndex() {
    return rowIndex;
  }

  @Override
  public Integer getColumnIndex() {
    return columnIndex;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }
}
