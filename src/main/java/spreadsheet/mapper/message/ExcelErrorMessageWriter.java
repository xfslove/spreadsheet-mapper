package spreadsheet.mapper.message;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.f2w.WorkbookReadException;
import spreadsheet.mapper.model.message.ErrorMessage;
import spreadsheet.mapper.w2f.WorkbookWriteException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * error message writer to excel decorator
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class ExcelErrorMessageWriter implements ErrorMessageWriter {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelErrorMessageWriter.class);

  private Map<String, MessageWriteStrategy> strategy2writeStrategy = new HashMap<>();

  private Workbook workbook;

  {
    messageWriteStrategy(
        new SingleCommentInCellStrategy(),
        new SingleTextBoxInSheetStrategy());
  }

  /**
   * this will create a new excel workbook to write error messages
   *
   * @param xlsx true use {@link XSSFWorkbook} else use {@link HSSFWorkbook}
   */
  public ExcelErrorMessageWriter(boolean xlsx) {
    workbook = xlsx ? new XSSFWorkbook() : new HSSFWorkbook();
  }

  /**
   * this will copy a excel workbook from supplied input stream to write error messages
   *
   * @param inputStream auto close
   */
  public ExcelErrorMessageWriter(InputStream inputStream) {
    try {
      workbook = WorkbookFactory.create(inputStream);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookReadException(e);
    }
  }

  @Override
  public ErrorMessageWriter messageWriteStrategy(MessageWriteStrategy... messageWriteStrategies) {
    if (messageWriteStrategies == null) {
      return this;
    }
    for (MessageWriteStrategy messageWriteStrategy : messageWriteStrategies) {
      strategy2writeStrategy.put(messageWriteStrategy.getStrategy(), messageWriteStrategy);
    }
    return this;
  }

  @Override
  public void write(Collection<ErrorMessage> errorMessages, OutputStream outputStream) {
    Map<String, Collection<ErrorMessage>> messageWriteStrategyMap = buildMessageWriteStrategyMap(errorMessages);
    for (String writeStrategy : messageWriteStrategyMap.keySet()) {
      MessageWriteStrategy messageWriteStrategy = strategy2writeStrategy.get(writeStrategy);
      if (messageWriteStrategy == null) {
        throw new WorkbookWriteException("no message write helper of [" + writeStrategy + "]");
      }

      messageWriteStrategy.write(workbook, messageWriteStrategyMap.get(writeStrategy));
    }

    try {
      workbook.write(outputStream);
    } catch (IOException e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookWriteException(e);
    } finally {

      try {
        workbook.close();
      } catch (IOException e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
      }
    }
  }

  private Map<String, Collection<ErrorMessage>> buildMessageWriteStrategyMap(Collection<ErrorMessage> errorMessages) {
    Map<String, Collection<ErrorMessage>> messageWriteStrategyMap = new HashMap<>();

    for (ErrorMessage errorMessage : errorMessages) {
      String messageWriteStrategy = errorMessage.getMessageWriteStrategy();

      if (!messageWriteStrategyMap.containsKey(messageWriteStrategy)) {
        messageWriteStrategyMap.put(messageWriteStrategy, new ArrayList<ErrorMessage>());
      }
      messageWriteStrategyMap.get(messageWriteStrategy).add(errorMessage);
    }

    return messageWriteStrategyMap;
  }
}
